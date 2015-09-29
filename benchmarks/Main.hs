import Control.Concurrent
import Control.Monad
import Criterion.Main

import qualified Control.Concurrent.Chan.Bounded.Batched as CBB

import Chan

main :: IO ()
main = do
    chan <- newChan
    forever $ writeChan chan 0

    bchan <- CBB.newChan 10000
    forever $ CBB.writeChan bchan 0

    threadDelay (5 * 10^5)

    defaultMain [
        bench "Chan.read" $ whnfIO (Chan.read chan)
      , bench "Chan.read1" $ whnfIO (Chan.read1 bchan)
      , bench "Chan.readBatch1" $ whnfIO (Chan.readBatch1 bchan)
      ]
