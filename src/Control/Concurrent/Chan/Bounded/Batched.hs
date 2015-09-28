module Control.Concurrent.Chan.Bounded.Batched (
    Chan
  , newChan
  , readChan
  , tryReadChan
  , readBatchChan
  , writeChan
  ) where

import Data.IORef
import Data.Sequence

import qualified Control.Concurrent.Chan.Unagi.Bounded as CB

data Chan a = Chan {
    inChan      :: CB.InChan a
  , outChan     :: CB.OutChan a
  , nextPending :: IORef (Maybe (IO a))
  }

newChan :: Int -> IO (Chan a)
newChan size = do
    (ichan, ochan) <- CB.newChan size
    ref <- newIORef Nothing
    return $ Chan ichan ochan ref

readNextPending :: Chan a -> IO (Maybe a)
readNextPending Chan { nextPending = ref } = do
    pending <- readIORef ref
    case pending of
        Just getNext -> do writeIORef ref Nothing
                           Just <$> getNext
        Nothing -> return Nothing

readChan :: Chan a -> IO a
readChan bchan@Chan { outChan = ochan } = do
    pending <- readNextPending bchan
    case pending of
        Just e  -> return e
        Nothing -> CB.readChan ochan

tryReadChan :: Chan a -> IO (Maybe a)
tryReadChan bchan@Chan { outChan = ochan, nextPending = ref } = do
    pending <- readNextPending bchan
    case pending of
        Just e  -> return $ Just e
        Nothing -> do (future, pending') <- CB.tryReadChan ochan
                      ready <- CB.tryRead future
                      case ready of
                          Just e  -> return $ Just e
                          Nothing -> do writeIORef ref $ Just pending'
                                        return Nothing

writeChan :: Chan a -> a -> IO ()
writeChan Chan { inChan = ichan } = CB.writeChan ichan

readBatchChan :: (a -> b -> (Bool, b)) -> b -> Chan a -> IO (Seq a)
readBatchChan stopIf initSt bchan = do
    e <- readChan bchan
    stop e initSt (singleton e)
  where stop e st batch = case stopIf e st of
                              (True, _)    -> return batch
                              (False, st') -> go st' batch
        go st batch = do r <- tryReadChan bchan
                         case r of
                             Just e  -> stop e st (batch |> e)
                             Nothing -> return batch
