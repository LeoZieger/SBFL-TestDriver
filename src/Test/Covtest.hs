{-|
Module      : Test.Covtest

A testsuite driver with support for per-test coverage information.
-}
module Test.Covtest ( writeInSeperateFile, Test (..), runTests ) where

import Trace.Hpc.Tix (writeTix)
import Trace.Hpc.Reflect (clearTix, examineTix)

tixFilePrefix :: String
tixFilePrefix = "./test/Test"

-- | Test with name and result
data Test = Test String -- ^ Name of Test
                 (IO Bool) -- ^ Result of Test


-- | Run multiple tests
runTests :: [Test] -- ^ List of Tests
         -> IO ()
runTests ((Test name result):ts) = do p <- result
                                      writeInSeperateFile name p
                                      runTests ts


-- | Run a test.
writeInSeperateFile :: String -- ^ The name of the test.
                    -> Bool -- ^ The result of the test.
                    -> IO ()
writeInSeperateFile n p = do currentTix <- examineTix
                             writeTix (tixFilePrefix ++
                                       show n ++
                                       (if p then "_PASS" else "_FAIL") ++
                                       ".tix") currentTix
                             clearTix