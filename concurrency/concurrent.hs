import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever)

import System.Console.Readline (readline)

main = do
  c <- newChan
  forkIO $ forever $ do
    x <- readChan c
    putStrLn $ "recv 1: " ++ x
  forkIO $ forever $ do
    x <- readChan c
    putStrLn $ "recv 2: " ++ x
  forever $ do
    l <- readline "-$> "
    case l of
      Nothing -> return ()
      Just x -> do
        putStrLn $ "send: " ++ x
        writeChan c x
