import Data.List
import System.Environment
import System.Exit
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
        _      -> do hPrintf stderr "Usage: %s %s\n" prog $ intercalate "|" $ map fst tests
                     exitFailure

runTest :: String -> IO ()
runTest name = do
    case lookup name tests of
        Nothing   -> do hPutStrLn stderr "Error: invalid specified test"
                        exitFailure
        Just test -> do hPrintf stderr "Running %s test...\n" name
                        test

tests :: [(String, IO ())]
tests = [
    ("BatchedChanTryRead", BatchedChanTryRead.run)
  , ("BatchedChanReadBatch", BatchedChanReadBatch.run)
    ]
