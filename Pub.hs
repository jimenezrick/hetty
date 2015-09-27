{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Pub where

import Data.Word
import Data.Foldable
import Data.IORef
import Data.ByteString
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan.Unagi.Bounded as Chan
import System.Socket
import System.Socket.Family.Inet

readSize = 4096

queueSize = 1000


data Conn = Conn { inChan :: InChan ByteString
                      , outChan :: OutChan ByteString
                      , sock :: Socket Inet Stream TCP
                      }

newConn :: Socket Inet Stream TCP -> IO Conn
newConn sock = do (inChan, outChan) <- Chan.newChan queueSize
                  return Conn { inChan = inChan
                              , outChan = outChan
                              ,  sock = sock
                              }
                   


newtype ContextRef a = ContextRef { unContextRef :: IORef Context } deriving Functor

data Context = Context { conns :: [Conn] }

-- TODO: monadic interface?
withContext :: ContextRef a -> (Context -> IO ()) -> IO ()
withContext ref f = do ctx <- readIORef (unContextRef ref)
                       f ctx



newContext :: IO (ContextRef a)
newContext = newIORef (Context []) >>= return . ContextRef

updateContext :: (Context -> Context) -> ContextRef a -> IO ()
updateContext update ref = atomicModifyIORef' (unContextRef ref) (\ctx -> (update ctx, ()))

addConn :: Conn -> Context -> Context
addConn conn ctx@Context{conns = conns} = ctx { conns = conn:conns }


publish :: ByteString -> Context -> IO ()
publish msg ctx = traverse_ (push msg) (conns ctx)
  where push msg conn = do done <- Chan.tryWriteChan (inChan conn) msg
                           if done
                               then return ()
                               else print "Dropping message"



connectPort :: ContextRef a -> Word16 -> IO ()
connectPort ref port = do
    sock <- socket :: IO (Socket Inet Stream TCP)
    let addr = SocketAddressInet loopback $ Port port
    connect sock addr

    conn <- newConn sock
    updateContext (addConn conn) ref

    runConn conn

    void $ send sock "hello\n" mempty





--
-- TODO: buffer and user blaze/builder for 4096 bytes per call
--
runConn :: Conn -> IO ()
runConn conn = void $ forkIO $ do
    print "Conn started"
    forever $ do
    msg <- Chan.readChan o
    send s msg mempty
  where s = sock conn
        o = outChan conn

