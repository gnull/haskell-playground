import Control.Concurrent
import qualified Control.Concurrent.ThreadManager as TM

main = do
  m <- TM.make
  TM.forkn m 10 $ do
    threadDelay 10000000
    putStrLn "kek"
  TM.waitForAll m
