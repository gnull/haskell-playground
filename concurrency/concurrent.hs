import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Chan
import Control.Monad (forever)

import System.Console.Readline (readline)

main = do
  c <- newChan
  mapM forkIO $ take 10 $ repeat $ forever $ do
    x <- readChan c
    t <- myThreadId
    putStrLn $ "recv " ++ show t ++ ": " ++ x
  forever $ do
    l <- readline "-$> "
    case l of
      Nothing -> return ()
      Just x -> do
        putStrLn $ "send: " ++ x
        writeChan c x
