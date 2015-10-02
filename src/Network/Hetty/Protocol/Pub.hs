module Network.Hetty.Protocol.Pub where

import Data.Word
import Data.Foldable
import Data.IORef
import Data.ByteString.Lazy
import Control.Monad
import System.Socket
import System.Socket.Family.Inet

import Control.Concurrent hiding (Chan)
import Control.Concurrent.Chan.Bounded.Batched as CBB

-- XXX: Move to its own module
data Conn a = Conn {
    chan :: Chan a
  , sock :: Socket Inet Stream TCP
  }

newConn :: Socket Inet Stream TCP -> IO Conn a
newConn sock = do chan <- CBB.newChan
                  return Conn { chan = chan, sock = sock}


newtype ContextRef a = ContextRef {
    unContextRef :: IORef Context a
  } deriving Functor

data Context a = Context { conns :: [Conn a] }

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
  where push msg conn = do done <- CBB.tryWriteChan conn msg
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




activateConn :: Conn -> IO ()
activateConn conn = void $ forkIO $ do
    forever $ do
        batch <- CBB.readBatchChan $ chan conn
        send (sock conn) batch mempty
