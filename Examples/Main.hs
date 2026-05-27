module Main where

import Examples.AssertCheckTest
import Test.MuCheck.TestAdapter.AssertCheck

main = do
    assertCheckResult sortEmpty
    assertCheckResult sortSorted
    assertCheckResult sortRev
    assertCheckResult sortSame
    assertCheckResult sortNeg
