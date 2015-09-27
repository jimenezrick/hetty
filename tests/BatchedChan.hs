import Control.Concurrent
import Control.Monad

import Control.Concurrent.Chan.Bounded.Batched as CBB

main :: IO ()
main = do
    bchan <- newBatchedChan 100
    void $ forkIO $ produce bchan
    forever $ do
        r <- CBB.tryReadChan bchan
        case r of
            Nothing -> print "Empty"
            Just e -> print e

produce :: BatchedChan Int -> IO ()
produce chan = go 0
  where go n = do CBB.writeChan chan n
                  yield
                  go (n+1)
