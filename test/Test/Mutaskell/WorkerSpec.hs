-- | Tests for the worker workload transport: the JSON document a parent
-- process hands to each interpreter worker child so the child can evaluate
-- the parent's already-selected mutant without regenerating candidates.
module Test.Mutaskell.WorkerSpec where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import App.Worker
    ( Workload(..)
    , WorkloadBase(..)
    , decodeWorkload
    , decodeWorkloadFile
    , encodeWorkload
    , mutantWorkload
    )
import Test.Mutaskell.Config (MuVar(..))
import Test.Mutaskell.TestAdapter (Mutant(..))
import Test.Mutaskell.Tix (toSpan)

base :: WorkloadBase
base = WorkloadBase
    { wbTarget      = "Examples/AssertCheckTest.hs"
    , wbTests       = ["assertCheckResult test_sortEmpty"]
    , wbTimeout     = Just 60000000
    , wbKeepMutants = Nothing
    , wbTestArgs    = ["--seed", "42"]
    }

mutant :: Mutant
mutant = Mutant
    { _mutant = "module Examples.AssertCheckTest where\nuncoveredDummy a = 0 + a\n"
    , _mtype  = MutateOther "negate-literal"
    , _mspan  = toSpan (12, 1, 13, 22)
    }

workload :: Workload
workload = mutantWorkload base mutant

-- | A hand-built workload document in the same shape 'encodeWorkload' emits.
-- Entries in @overrides@ replace top-level keys, so malformed variants can be
-- constructed without duplicating the whole document.
workloadDoc :: [(String, A.Value)] -> String
workloadDoc overrides = BL.unpack (A.encode obj)
  where
    obj = foldl (\m (k, v) -> KM.insert (Key.fromString k) v m) baseKm overrides
    baseKm = case A.decode (BL.pack (encodeWorkload workload)) of
        Just km -> km
        Nothing -> error "worker spec: could not re-parse an encoded workload"

spec :: Spec
spec = describe "worker workload transport" $ do
    describe "mutantWorkload" $
        it "fills the workload from the base config and the selected mutant" $ do
            wTarget workload `shouldBe` "Examples/AssertCheckTest.hs"
            wMutant workload `shouldBe` mutant
            wTests workload `shouldBe` ["assertCheckResult test_sortEmpty"]
            wTimeout workload `shouldBe` Just 60000000
            wKeepMutants workload `shouldBe` Nothing
            wTestArgs workload `shouldBe` ["--seed", "42"]

    describe "encodeWorkload / decodeWorkload" $ do
        it "round-trips a workload" $
            decodeWorkload (encodeWorkload workload) `shouldBe` Right workload

        it "round-trips a workload without timeout or keep-dir" $ do
            let bare = workload { wTimeout = Nothing, wKeepMutants = Nothing, wTestArgs = [] }
            decodeWorkload (encodeWorkload bare) `shouldBe` Right bare

        it "rejects malformed JSON with a classified error" $
            classifyError (decodeWorkload "not json at all")

        it "rejects an unsupported transport version with a classified error" $
            classifyError (decodeWorkload (workloadDoc [("version", A.toJSON (2 :: Int))]))

        it "rejects an unknown mutator name with a classified error" $
            classifyError (decodeWorkload (workloadDoc [("mutator", A.toJSON "no-such-mutator")]))

        it "rejects a span that is not four coordinates" $
            classifyError (decodeWorkload (workloadDoc [("span", A.toJSON [1, 1 :: Int])]))

    describe "decodeWorkloadFile" $ do
        it "reads an encoded workload back from disk" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                let path = tmp ++ "/workload.json"
                writeFile path (encodeWorkload workload)
                result <- decodeWorkloadFile path
                result `shouldBe` Right workload

        it "classifies a missing workload file as an error" $
            classifyError =<< decodeWorkloadFile "/nonexistent/mucheck-workload-missing.json"
  where
    classifyError r = case r of
        Left err -> err `shouldContain` "worker:"
        Right _  -> expectationFailure "expected a classified error"