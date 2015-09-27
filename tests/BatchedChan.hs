import Control.Concurrent
import Control.Monad

import BatchedChan as BC

main :: IO ()
main = do
    bchan <- newBatchedChan 100
    void $ forkIO $ produce bchan
    forever $ do
        r <- BC.tryReadChan bchan
        case r of
            Nothing -> print "Empty"
            Just e -> print e

produce :: BatchedChan Int -> IO ()
produce chan = go 0
  where go n = do BC.writeChan chan n
                  yield
                  go (n+1)
