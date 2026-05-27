{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec

import qualified Debug046Spec
import qualified Test.MuCheck.Mutation
import qualified Test.MuCheck.Utils.Common
import qualified Test.MuCheck.Utils.Print

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Common" Test.MuCheck.Utils.CommonSpec.spec
    describe "Print" Test.MuCheck.Utils.PrintSpec.spec
    describe "Mutation" Test.MuCheck.MutationSpec.spec
    describe "Debug046" Debug046Spec.spec
