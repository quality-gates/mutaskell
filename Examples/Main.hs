module Main where

import Examples.AssertCheckTest
import Test.MuCheck.TestAdapter.AssertCheck

main :: IO ()
main = mapM_ assertCheckResult
    [ test_sortEmpty
    , test_sortSorted
    , prop_sortIsIdempotent
    , sortSame
    , test_sortNeg
    ]
