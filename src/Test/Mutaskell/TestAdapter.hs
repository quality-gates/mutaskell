{-# LANGUAGE FunctionalDependencies #-}

-- | Module for adapting test framekworks
module Test.Mutaskell.TestAdapter where

import Data.Typeable
import qualified Language.Haskell.Interpreter as I
import Test.Mutaskell.Mutant (Mutant)

-- | Wrapper for interpreter output
data (Summarizable a) => InterpreterOutput a = Io {_io :: Either I.InterpreterError a, _ioLog :: String}

-- | Holding test information
type TestStr = String

-- | Summary of test run
newtype Summary = Summary String
  deriving (Show, Read)

-- | Interface to be implemented by a test framework
class (Typeable s) => Summarizable s where
    -- | Summary of test suite on a single mutant
    testSummary :: Mutant -> TestStr -> InterpreterOutput s -> Summary

    -- | Was the test run a success
    isSuccess :: s -> Bool

    -- | Was the test run a failure
    isFailure :: s -> Bool
    isFailure = not . isSuccess

    -- | Was the test run neither (gaveup/timedout)
    isOther :: s -> Bool
    isOther x = not (isSuccess x) && not (isFailure x)

{- | Specify the kind of test suite to run. The result processing, and also
how the tests are run are based on this.
-}
class TRun a s | a -> s where
    {- | Generate a runnable test string out of passed in module name and
    annotated test function name.
    -}
    genTest :: a -> String -> TestStr

    getName :: a -> String
    toRun :: String -> a
