module Chan where

import Control.Monad
import Control.Concurrent

import qualified Control.Concurrent.Chan.Bounded.Batched as CBB

read :: Chan Int -> IO ()
read c = do writeChan c 0
            void $ readChan c

read1 :: CBB.Chan Int -> IO ()
read1 c = do CBB.writeChan c 0
             void $ CBB.readChan c

readBatch1 :: CBB.Chan Int -> IO ()
readBatch1 c = do CBB.writeChan c 0
                  void $ CBB.readBatchChan (\_ cnt -> (cnt == 1, undefined)) (1 :: Int) c
