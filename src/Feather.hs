{-# LANGUAGE TemplateHaskell #-}

module Feather
  ( EventHandler(..)
  , EventState
  , EventInput
  , HasEvents(..)
  , Complex(..)
  , Complex2
  , Complex3
  , emptyEventState
  , addHandler
  , runEvent
  , getHandler
  , testEvents
  ) where

import Control.Monad.State
import Control.Monad.State.Class
import Control.Concurrent
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Typeable
import Lens.Micro.Platform
import Unsafe.Coerce

--
-- | Types
--

class HasEvents s where
  lensEvents :: Lens' s EventState

newtype HandlerId = HandlerId Int deriving (Eq, Ord, Num, Show)

type Updater m = m ()
type Dispatcher i m = i -> m ()
type Builder i m = m (Dispatcher i m)
type InputSpec m = (TypeRep, OpaqueBuilder, m ())

-- Constraint aliases
type T a = Typeable a
type SM s m = (HasEvents s, MonadState s m)
type SIM s i m = (SM s m, T i)

data OpaqueBuilder where
  OpaqueBuilder :: SIM s i m => Builder i m -> OpaqueBuilder

data OpaqueDispatcher where
  OpaqueDispatcher :: SIM s i m => Dispatcher i m -> OpaqueDispatcher

data OpaqueUpdater where
  OpaqueUpdater :: SM s m => Updater m -> OpaqueUpdater

type EventHandler i o m = Dispatcher o m -> Dispatcher i m

emptyEventState :: EventState
emptyEventState = EventState 0 mempty mempty mempty mempty []

data EventState =
  EventState
    { eId'         :: HandlerId
    , eListeners'  :: Map TypeRep [(HandlerId, OpaqueBuilder)]
    , eHandlers'   :: Map HandlerId TypeRep
    , eEmitters'   :: Map TypeRep [(HandlerId, OpaqueUpdater)]
    , eDispatcher' :: Map TypeRep OpaqueDispatcher
    , eSeen'       :: [HandlerId]
    }

makeLensesWith abbreviatedFields ''EventState

-- | An event handler may listen for many different types
class SIM s i m => EventInput s i m where
  getInputs :: T o => EventHandler i o m -> m [InputSpec m]

instance {-# OVERLAPPABLE #-} SIM s i m => EventInput s i m where
  getInputs = pure . makeInput (id :: i -> i)

--
-- Add listener
--

addHandler :: forall s i m o. (SIM s i m, EventInput s i m, T o, T m)
           => EventHandler i o m -> m ()
addHandler h = do

  hid <- lensEvents . id' <<%= (+1)
  inputs <- getInputs h
  let update = forM_ inputs (^._3)

  -- Register handler
  lensEvents . handlers' . at hid ?= typeOf h

  -- Register inputs
  forM_ inputs $
    \(t, b, _) -> do
      lensEvents . listeners t %= ((hid, b):)

  -- Register output
  let oType = typeRep (Proxy :: Proxy o)
  lensEvents . emitters oType %= ((hid, OpaqueUpdater update):)

  -- Trigger update
  lensEvents . seen' .= [] >> update

-- | Build a dispatcher for a given type
buildDispatcher :: forall s i m. SIM s i m => m (i -> m ())
buildDispatcher = do
  let t = typeRep (Proxy :: Proxy i)
      toHandler (_, OpaqueBuilder b) = unsafeCoerce b
      runHandlers hs i = forM_ hs ($ i)
  fwds <- use $ lensEvents . listeners t
  runHandlers <$> forM fwds toHandler

-- | Get an event handler from the cache
getHandler :: forall s i m. SIM s i m => m (i -> m ())
getHandler = do
  let t = typeRep (Proxy :: Proxy i)
      f (OpaqueDispatcher h) = unsafeCoerce h
      n _ = pure ()
      l = lensEvents . dispatcher' . at t
  maybe n f <$> use l

-- | Update dispatchers from a given input type recursing upstream
updateDispatchers :: forall s i m. SIM s i m => i -> m ()
updateDispatchers i = do
  let t = typeOf i
  dispatch <- buildDispatcher :: m (i -> m ())
  lensEvents . dispatcher' . at t ?= OpaqueDispatcher dispatch
  -- Recurse
  es <- use $ lensEvents . emitters t
  forM_ es $
    \(hid, OpaqueUpdater u) -> do
      checkLoop hid >> unsafeCoerce u
  where
    -- Chances are this will not suffice
    checkLoop t = do
      seen <- lensEvents . seen' <<%= (t:)
      let e l = error $ "Cycle in events graph: " ++ show l
      maybe (pure ()) (\n -> e $ t : take (n+1) seen) $ elemIndex t seen

-- | Make a specification of an input closing over it's type
makeInput :: forall s i o m a. (SIM s i m, T a, T o)
          => (a -> i) -> EventHandler i o m -> [InputSpec m]
makeInput w f =
  let a = undefined :: a
      t = typeOf a
      h y = f y . w
      builder = h <$> getHandler
      r = (t, OpaqueBuilder builder, updateDispatchers a)
   in if t == typeOf () then [] else [r]

runEvent :: SIM s i m => i -> m ()
runEvent i = getHandler >>= ($ i)

--
-- | Complex events
--

data Complex a b c = C1 a | C2 b | C3 c
  deriving (Eq, Ord, Show)

type Complex2 a b = Complex a b ()
type Complex3 a b c = Complex a b c

instance (SM s m, T a, T b, T c) => EventInput s (Complex a b c) m where
  getInputs h = pure $ concat [w C1, w C2, w C3]
    where
      w :: T i => (i -> Complex a b c) -> [InputSpec m]
      w c = makeInput c h

--
-- | Lenses
--

instance HasEvents EventState where
  lensEvents = id

nonl :: Lens' (Maybe [a]) [a]
nonl afb s = f <$> afb (fromMaybe [] s)
  where f y = if null y then Nothing else Just y

listeners :: TypeRep -> Lens' (EventState) [(HandlerId, OpaqueBuilder)]
listeners t = listeners' . at t . nonl

emitters :: TypeRep -> Lens' (EventState) [(HandlerId, OpaqueUpdater)]
emitters t = emitters' . at t . nonl


type TestM = StateT EventState IO
type TestHandler i o = EventHandler i o TestM

data A = A
data B = B
data C = C
data Done = Done

handler1 :: a -> ((o -> TestM ()) -> a -> i -> TestM a) -> TestM (TestHandler i o)
handler1 empty f = f' <$> liftIO (newMVar empty)
  where
    f' mvar y i = do
      a <- liftIO $ takeMVar mvar
      f y a i >>= liftIO . putMVar mvar

collector :: TestM (TestHandler (Complex2 C Done) ())
collector = handler1 0 f
  where
    f y a (C1 C) = pure $ a + 1
    f y a (C2 Done) = liftIO (print a) >> pure 0

makeTestHandler :: a -> b -> Int -> TestHandler a b
makeTestHandler a b i = f
  where f y a = replicateM_ i (y b)

testEvents :: IO ()
testEvents = do
  _ <- runStateT act emptyEventState
  pure ()
  where
    act :: StateT EventState IO ()
    act = do
      addHandler $ makeTestHandler A B 10
      addHandler $ makeTestHandler B C 10
      addHandler $ makeTestHandler C A 10
      addHandler =<< collector
      runEvent A
      runEvent Done

