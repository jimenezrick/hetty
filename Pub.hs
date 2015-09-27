{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

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
                   


newtype ContextRef a = ContextRef { getContextRef :: IORef Context } deriving Functor

data Context = Context { conns :: [Conn] }



newContext :: IO (ContextRef a)
newContext = newIORef (Context []) >>= return . ContextRef

updateContext :: (Context -> Context) -> ContextRef a -> IO ()
updateContext update ctx = modifyIORef' (getContextRef ctx) update

addConn :: Conn -> Context -> Context
addConn conn ctx@Context{conns = conns} = ctx { conns = conn:conns }


publish :: Context -> ByteString -> IO ()
publish ctx msg = traverse_ (push msg) (conns ctx)
  where push msg conn = do full <- tryWriteChan (inChan conn) msg
                           if full
                               then print "Dropping message"
                               else return ()



connectPort :: ContextRef a -> Word16 -> IO ()
connectPort ref port = do
    sock <- socket :: IO (Socket Inet Stream TCP)
    let addr = SocketAddressInet loopback $ Port port
    connect sock addr

    conn <- newConn sock
    updateContext (addConn conn) ref

    void $ send sock "hello\n" mempty





runConn :: Conn -> IO ()
runConn conn = void $ forkIO $ do
    forever $ do
    msg <- Chan.readChan o
    send s msg mempty
  where s = sock conn
        o = outChan conn

