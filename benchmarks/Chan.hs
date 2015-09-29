module Chan where

import Control.Monad
import Control.Concurrent

import qualified Control.Concurrent.Chan.Bounded.Batched as CBB

read :: Chan Int -> IO ()
read = void . readChan

read1 :: CBB.Chan Int -> IO ()
read1 = void . CBB.readChan

readBatch1 :: CBB.Chan Int -> IO ()
readBatch1 c = void $ CBB.readBatchChan (\_ cnt -> (cnt == 1, undefined)) (1 :: Int) c
