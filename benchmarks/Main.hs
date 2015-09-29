import Control.Concurrent
import Criterion.Main

import qualified Control.Concurrent.Chan.Bounded.Batched as CBB

import Chan

main :: IO ()
main = do
    chan <- newChan
    bchan <- CBB.newChan 100

    defaultMain [
        bench "Chan.read" $ whnfIO (Chan.read chan)
      , bench "Chan.read1" $ whnfIO (Chan.read1 bchan)
      , bench "Chan.readBatch1" $ whnfIO (Chan.readBatch1 bchan)
      ]
