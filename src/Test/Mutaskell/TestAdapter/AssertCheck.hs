-- | Module for using demonstration of using the TestAdapter
module Test.Mutaskell.TestAdapter.AssertCheck where

import Control.Exception

-- | Result of an assertion check
data AssertStatus
    = AssertSuccess -- ^ Assertion passed
    | AssertFailure -- ^ Assertion failed
    deriving (Eq, Show)

-- | Convert a boolean to an AssertStatus
assertCheck :: Bool -> AssertStatus
assertCheck fn = case fn of
    True -> AssertSuccess
    False -> AssertFailure

-- | Print the result of an assertion check and handle exceptions
assertCheckResult :: AssertStatus -> IO AssertStatus
assertCheckResult fn = withCheck $ case fn of
    AssertSuccess -> do
        putStrLn "Success"
        return AssertSuccess
    AssertFailure -> do
        putStrLn "Failed"
        return AssertFailure
  where
    withCheck f = catch f $ \e -> do
        print (e :: SomeException)
        return AssertFailure
