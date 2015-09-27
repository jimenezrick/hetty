{-# LANGUAGE OverloadedStrings #-}

-- Build:
-- $ ghc --make -eventlog -rtsopts -threaded -O2 hetty
--
-- Run:
-- $ time ./hetty +RTS -A10M -qg -l
--
-- Profile:
-- $ ghc-events-analyze -b 10000 hetty.eventlog
-- $ perf record --call-graph dwarf -- ./hetty +RTS -A10M -qg
-- $ perf report

import qualified Data.ByteString.Char8 as B8

import qualified GHC.Conc as GC
import System.Socket
import System.Socket.Family.Inet

import Control.Monad
import qualified Control.Concurrent as C
import Control.Concurrent.Chan.Unagi.Bounded

main = do
    cpus <- GC.getNumProcessors
    C.setNumCapabilities cpus
    cap <- C.getNumCapabilities
    print $ "Capabilities: " ++ show (cap, cpus)

    sock <- socket :: IO (Socket Inet Stream TCP)
    let addr = SocketAddressInet loopback 8000
    connect sock addr

    send sock "hello\n" mempty

    (ci, co) <- newChan (10^3 :: Int)

    C.forkIO $ sender ci msg100
    receiver co sock

    close sock

receiver co sock = go (4 * 10^7 :: Int) []
  where go 0 []                      = return ()
        go n buf | length buf == 40  = send sock (B8.concat buf) mempty >> go n []
        go n buf                     = do m <- readChan co
                                          go (n-1) (m:buf)
-- XXX tryReadChan :: OutChan a -> IO (Element a, IO a)

sender ci msg = doN (4 * 10^7 :: Int) $ writeChan ci msg

msg n = B8.pack $ replicate (n-1) '0' ++ ['\n']

msg10   = msg 10
msg100  = msg 100
msg1000 = msg 1000

doN 0 _ = return ()
doN n f = f >> doN (n-1) f
