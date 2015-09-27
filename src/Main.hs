import Control.Monad

import qualified Data.ByteString.Char8 as B8

import qualified TCP
import qualified Transport
import qualified Pub

main = do ref <- Pub.newContext
          Pub.connectPort ref 8000
          Pub.connectPort ref 8001
          Pub.connectPort ref 8002
          Pub.connectPort ref 8003

          Pub.withContext ref $ \ctx ->
            doN (10^7) $ Pub.publish msg100 ctx

doN 0 _ = return ()
doN n f = f >> doN (n-1) f

msg n = B8.pack $ replicate (n-1) '0' ++ ['\n']

msg10   = msg 10
msg100  = msg 100
