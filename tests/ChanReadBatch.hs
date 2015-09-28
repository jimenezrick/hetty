module ChanReadBatch where

import Control.Concurrent
import Control.Monad

import Control.Concurrent.Chan.Bounded.Batched as CBB

run :: IO ()
run = do
    bchan <- CBB.newChan 10
    void $ forkIO $ produce bchan
    forever $ do
        batch <- CBB.readBatchChan (\_ cnt -> (cnt == 10, cnt+1)) (1 :: Int) bchan
        print batch

produce :: CBB.Chan Int -> IO ()
produce chan = go 0
  where go n = do CBB.writeChan chan n
                  yield
                  go (n+1)
