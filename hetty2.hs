{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B8

import qualified GHC.Conc as GC
import Network.Socket
import qualified Network.Socket.ByteString as SB

import Control.Monad
import qualified Control.Concurrent as C
import Control.Concurrent.Chan.Unagi

main = do
    cpus <- GC.getNumProcessors
    C.setNumCapabilities cpus
    cap <- C.getNumCapabilities
    print $ "Capabilities: " ++ show (cap, cpus)

    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr "127.0.0.1"
    connect sock $ SockAddrInet 8000 addr

    send sock "hello\n"

    (ci, co) <- newChan

    C.forkIO $ sender ci msg100
    receiver co sock

    close sock

receiver co sock = go (10^7 :: Int) []
  where go 0 []                      = return ()
        go n buf | length buf == 40  = SB.sendMany sock buf >> go n []
        go n buf                     = do m <- readChan co
                                          go (n-1) (m:buf)
-- XXX tryReadChan :: OutChan a -> IO (Element a, IO a)

sender ci msg = doN (10^7 :: Int) $ writeChan ci msg

msg n = B8.pack $ replicate (n-1) '0' ++ ['\n']

msg10   = msg 10
msg100  = msg 100
msg1000 = msg 1000

doN 0 _ = return ()
doN n f = f >> doN (n-1) f
