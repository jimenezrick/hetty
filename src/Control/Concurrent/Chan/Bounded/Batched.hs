module Control.Concurrent.Chan.Bounded.Batched (
    Chan
  , newChan
  , readChan
  , tryReadChan
  , readBatchChan
  , writeChan
  , tryWriteChan
  ) where

import Prelude hiding (length)
import Data.ByteString
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.IORef

import qualified Data.ByteString.Lazy as BL

import qualified Control.Concurrent.Chan.Unagi.Bounded as CB

data Chan = Chan {
    inChan      :: CB.InChan ByteString
  , outChan     :: CB.OutChan ByteString
  , nextPending :: IORef (Maybe (IO ByteString))
  }

newChan :: Int -> IO Chan
newChan size = do
    (ichan, ochan) <- CB.newChan size
    ref <- newIORef Nothing
    return $ Chan ichan ochan ref

readNextPending :: Chan -> IO (Maybe ByteString)
readNextPending Chan { nextPending = ref } = do
    pending <- readIORef ref
    case pending of
        Just getNext -> do writeIORef ref Nothing
                           Just <$> getNext
        Nothing -> return Nothing

readChan :: Chan -> IO ByteString
readChan bchan@Chan { outChan = ochan } = do
    pending <- readNextPending bchan
    case pending of
        Just e  -> return e
        Nothing -> CB.readChan ochan

tryReadChan :: Chan -> IO (Maybe ByteString)
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

writeChan :: Chan -> ByteString -> IO ()
writeChan = CB.writeChan . inChan

tryWriteChan :: Chan -> ByteString -> IO Bool
tryWriteChan = CB.tryWriteChan . inChan

readBatchChan :: Chan -> IO BL.ByteString
readBatchChan bchan = do
    e <- readChan bchan
    stop (length e) $ byteString e
  where stop size batch | size >= batchSize = return $ concatBatch batch
                        | otherwise         = go size batch
        go size batch = do r <- tryReadChan bchan
                           case r of
                               Just e  -> stop (size + length e) (batch `mappend` byteString e)
                               Nothing -> return $ concatBatch batch

batchSize :: Int
batchSize = 4096

concatBatch :: Builder -> BL.ByteString
concatBatch = toLazyByteStringWith
    (untrimmedStrategy smallChunkSize defaultChunkSize) BL.empty
