import Data.List
import System.Environment
import System.IO
import Text.Printf

import BatchedChanTryRead
import BatchedChanReadBatch

main :: IO ()
main = do
    prog <- getProgName
    args <- getArgs
    case args of
        [test] -> runTest test
        _      -> hPrintf stderr "Usage: %s %s\n" prog $ intercalate "|" $ map fst tests

runTest :: String -> IO ()
runTest name = do
    case lookup name tests of
        Nothing   -> hPutStrLn stderr "Error: invalid specified test"
        Just test -> do hPrintf stderr "Running %s\n" name
                        test

tests :: [(String, IO ())]
tests = [
    ("BatchedChanTryRead", BatchedChanTryRead.run)
  , ("BatchedChanReadBatch", BatchedChanReadBatch.run)
    ]
