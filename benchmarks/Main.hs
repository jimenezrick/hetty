import Criterion.Main

import Chan

main :: IO ()
main = defaultMain [
    bench "Chan" $ whnfIO Chan.run
    ]
