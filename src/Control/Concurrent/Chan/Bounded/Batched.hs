module Control.Concurrent.Chan.Bounded.Batched (
    BatchedChan
  , newBatchedChan
  , readChan
  , tryReadChan
  , readBatchChan
  , writeChan
  ) where

import Data.IORef
import Data.Sequence

import qualified Control.Concurrent.Chan.Unagi.Bounded as CB

data BatchedChan a = BatchedChan {
    inChan      :: CB.InChan a
  , outChan     :: CB.OutChan a
  , nextPending :: IORef (Maybe (IO a))
  }

newBatchedChan :: Int -> IO (BatchedChan a)
newBatchedChan size = do
    (ichan, ochan) <- CB.newChan size
    ref <- newIORef Nothing
    return $ BatchedChan ichan ochan ref

readNextPending :: BatchedChan a -> IO (Maybe a)
readNextPending BatchedChan { nextPending = ref } = do
    pending <- readIORef ref
    case pending of
        Just getNext -> do writeIORef ref Nothing
                           Just <$> getNext
        Nothing -> return Nothing

readChan :: BatchedChan a -> IO a
readChan bchan@BatchedChan { outChan = ochan } = do
    pending <- readNextPending bchan
    case pending of
        Just e  -> return e
        Nothing -> CB.readChan ochan

tryReadChan :: BatchedChan a -> IO (Maybe a)
tryReadChan bchan@BatchedChan { outChan = ochan, nextPending = ref } = do
    pending <- readNextPending bchan
    case pending of
        Just e  -> return $ Just e
        Nothing -> do (future, pending') <- CB.tryReadChan ochan
                      ready <- CB.tryRead future
                      case ready of
                          Just e  -> return $ Just e
                          Nothing -> do writeIORef ref $ Just pending'
                                        return Nothing

writeChan :: BatchedChan a -> a -> IO ()
writeChan BatchedChan { inChan = ichan } = CB.writeChan ichan

readBatchChan :: (a -> b -> (Bool, b)) -> b -> BatchedChan a -> IO (Seq a)
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
