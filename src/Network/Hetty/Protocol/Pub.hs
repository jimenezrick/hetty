module Network.Hetty.Protocol.Pub where

import Data.Foldable
import Data.IORef
import Data.ByteString hiding (map)
import Control.Concurrent hiding (Chan)
import Control.Monad

import Control.Concurrent.Chan.Bounded.Batched as CBB
import Network.Hetty.Transport

-- XXX: Move to its own module
data Conn = Conn {
    chan      :: Chan
  , transport :: Transport
  }

chanSize :: Int
chanSize = 1024

newConn :: Transport -> IO Conn
newConn tport = do c <- CBB.newChan chanSize
                   return Conn { chan = c, transport = tport }

newtype ContextRef = ContextRef {
    unContextRef :: IORef Context
  }

data Context = Context { conns :: [Conn] }

-- TODO: monadic interface?
withContext :: ContextRef -> (Context -> IO ()) -> IO ()
withContext ref f = do ctx <- readIORef (unContextRef ref)
                       f ctx



newContext :: IO ContextRef
newContext = newIORef (Context []) >>= return . ContextRef

updateContext :: (Context -> Context) -> ContextRef -> IO ()
updateContext update ref = atomicModifyIORef' (unContextRef ref) (\ctx -> (update ctx, ()))

addConn :: ContextRef -> Conn -> IO ()
addConn ref conn = do
    updateContext (\ctx@Context{conns = cs } -> ctx { conns = conn:cs }) ref


publish :: ByteString -> Context -> IO ()
publish msg ctx = traverse_ (push msg) $ map chan $ conns ctx
  where push msg conn = do done <- CBB.tryWriteChan conn msg
                           if done
                               then return ()
                               else print "Dropping message"







activateConn :: Conn -> IO ()
activateConn conn = void $ forkIO $ do
    forever $ do
        batch <- CBB.readBatchChan $ chan conn
        send (transport conn) batch
