module Small where

import Test.Mutaskell.TestAdapter.AssertCheck

flag001 = True

test_g1 = assertCheck (flag001)
