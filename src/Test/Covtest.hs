{-|
Module      : Test.Covtest

A testsuite driver with support for per-test coverage information.
-}
module Test.Covtest ( writeInSeperateFile ) where

import Trace.Hpc.Tix (writeTix)
import Trace.Hpc.Reflect (clearTix, examineTix)

tixFilePrefix :: String
tixFilePrefix = "./test/Test"

-- | Run a test.
writeInSeperateFile :: Int -- ^ The number of the test.
                    -> Bool -- ^ The result of the test.
                    -> IO ()
writeInSeperateFile n t = do currentTix <- examineTix
                             writeTix (tixFilePrefix ++
                                       show n ++
                                       (if t then "_PASS" else "_FAIL") ++
                                       ".tix") currentTix
                             clearTix