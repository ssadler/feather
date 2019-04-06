# Feather

A minimalistic and performant events dispatcher.

Feather builds a tree of event dispatchers before events are dispatched, effectively allowing you to do this:

```haskell

-- Builds a small event graph. The interesting bit is that after we have referenced our dispatcher,
-- the event state is no longer required.
--
-- prints:
-- 1
-- 1


type TestM = StateT EventState IO

main :: IO ()                                                 
main = do                                                     
  flip runStateT emptyEventState $ do                         
    addHandler showHandler                                    
    addHandler printHandler                                   
    addHandler printHandler                                   
    dispatch <- getHandler                                    
    put $ error "will not be reached, because dispatch\
                \encapsulates the whole downstream DAG"                         
    dispatch (1::Int)                                         

  pure ()
  where
    showHandler :: EventHandler Int String TestM
    showHandler yield = yield . show

    printHandler :: EventHandler String () TestM
    printHandler _ = liftIO . putStrLn

```
