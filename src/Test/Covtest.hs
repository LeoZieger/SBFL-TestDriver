{-|
Module      : Test.Covtest

A testsuite driver with support for per-test coverage information.
-}
module Test.Covtest ( writeInSeperateFile, Test (..), runTests ) where

import Trace.Hpc.Tix (writeTix)
import Trace.Hpc.Reflect (clearTix, examineTix)
import System.FilePath ( (<.>), (</>) )


-- | Test with name and result
data Test = Test String -- ^ Name of Test
                 Bool -- ^ Result of Test


-- | Run multiple tests
runTests :: [Test] -- ^ List of Tests
         -> FilePath -- ^ Folder where to place tix files
         -> IO ()
runTests ts path = do print ("Running " ++ show (length ts) ++ " Tests")
                      go ts
                      print "Done"
    where
        go :: [Test] -> IO ()
        go [] = pure ()
        go ((Test name result):ts') = do print (name ++ " | " ++ show result)
                                         writeInSeperateFile name result path
                                         go ts'


-- | Run a test.
writeInSeperateFile :: String -- ^ The name of the test.
                    -> Bool -- ^ The result of the test.
                    -> FilePath -- ^ Folder where to place tix files
                    -> IO ()
writeInSeperateFile n p path = do currentTix <- examineTix
                                  print $ "Writing to " ++ path
                                  writeTix (path </> (n ++ (if p then "_PASS" else "_FAIL") <.> "tix")) currentTix
                                  clearTix
