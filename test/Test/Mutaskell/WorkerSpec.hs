-- | Tests for the worker workload transport: the JSON document a parent
-- process hands to each interpreter worker child so the child can evaluate
-- the parent's already-selected mutant without regenerating candidates.
module Test.Mutaskell.WorkerSpec where

import Control.Exception (bracket_)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import App.Worker
    ( Workload(..)
    , WorkloadBase(..)
    , decodeWorkload
    , decodeWorkloadFile
    , encodeWorkload
    , evalWorkload
    , mutantWorkload
    , runWorkloadMode
    , workerDeserialize
    )
import Test.Mutaskell.Config (MuVar(..))
import Test.Mutaskell.Interpreter (MutantSummary(..), isSkippedSummary)
import Test.Mutaskell.TestAdapter (Mutant(..))
import Test.Mutaskell.Tix (toSpan)
import Test.Mutaskell.Utils.Common (hash)

base :: WorkloadBase
base = WorkloadBase
    { wbTarget      = "Examples/AssertCheckTest.hs"
    , wbTests       = testStrings
    , wbTimeout     = Nothing
    , wbKeepMutants = Nothing
    , wbTestArgs    = []
    }

-- | Every test in the example module, built the way the parent builds them.
testStrings :: [String]
testStrings = map ("assertCheckResult " ++)
    [ "test_sortEmpty", "test_sortSorted", "prop_sortIsIdempotent"
    , "sortSame", "test_sortNeg"
    ]

mutant :: Mutant
mutant = Mutant
    { _mutant = "module Examples.AssertCheckTest where\nuncoveredDummy a = 0 + a\n"
    , _mtype  = MutateOther "negate-literal"
    , _mspan  = toSpan (12, 1, 13, 22)
    }

workload :: Workload
workload = mutantWorkload base mutant

-- | Replace a source line in the example module, standing in for a generated
-- mutant of it.
editedExample :: String -> String -> IO String
editedExample from to = do
    src <- readFile "Examples/AssertCheckTest.hs"
    return $ unlines $ map (\l -> if l == from then to else l) (lines src)

workloadOn :: FilePath -> String -> Workload
workloadOn target src = workload
    { wTarget = target
    , wMutant = mutant { _mutant = src }
    }

killedWorkload :: IO Workload
killedWorkload = workloadOn "Examples/AssertCheckTest.hs"
    <$> editedExample "qsort [] = []" "qsort [] = [0]"

aliveWorkload :: IO Workload
aliveWorkload = workloadOn "Examples/AssertCheckTest.hs"
    <$> editedExample "uncoveredDummy a = 0 + a" "uncoveredDummy a = 1 + a"

invalidWorkload :: IO Workload
invalidWorkload = do
    src <- readFile "Examples/AssertCheckTest.hs"
    return $ workloadOn "Examples/AssertCheckTest.hs" (src ++ "\nbroken = = =\n")

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

-- | Assert a summary carries the result tag under test, using the same tag
-- vocabulary as the worker IPC format.
assertTag :: String -> MutantSummary -> Expectation
assertTag tag s = case s of
    MSumKilled{}  -> "killed"  `shouldBe` tag
    MSumAlive{}   -> "alive"   `shouldBe` tag
    MSumError{}   -> "error"   `shouldBe` tag
    MSumSkipped{} -> "skipped" `shouldBe` tag
    MSumOther{}   -> "other"   `shouldBe` tag

classifyError :: Either String a -> Expectation
classifyError r = case r of
    Left err -> err `shouldContain` "worker:"
    Right _  -> expectationFailure "expected a classified error"

spec :: Spec
spec = describe "worker workload transport" $ do
    describe "mutantWorkload" $
        it "fills the workload from the base config and the selected mutant" $ do
            wTarget workload `shouldBe` "Examples/AssertCheckTest.hs"
            wMutant workload `shouldBe` mutant
            wTests workload `shouldBe` testStrings
            wTimeout workload `shouldBe` Nothing
            wKeepMutants workload `shouldBe` Nothing
            wTestArgs workload `shouldBe` []

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

    describe "evalWorkload" $ do
        it "classifies a killed mutant" $
            killedWorkload >>= evalWorkload >>= assertTag "killed"

        it "classifies a surviving mutant as alive" $
            aliveWorkload >>= evalWorkload >>= assertTag "alive"

        it "classifies a non-compilable mutant as skipped" $ do
            s <- evalWorkload =<< invalidWorkload
            isSkippedSummary s `shouldBe` True

        it "classifies a per-mutant timeout as an error" $ do
            wl <- killedWorkload
            s <- evalWorkload wl { wTimeout = Just 1 }
            case s of
                MSumError _ err _ -> err `shouldContain` "Timeout occurred"
                _                 -> expectationFailure "expected MSumError"

        it "evaluates the transported mutant without reading the target source" $ do
            -- The child receives everything it needs in the workload; the
            -- target path is a label only.  Pointing it at a missing file
            -- would break any child that still regenerated from the source.
            src <- editedExample "qsort [] = []" "qsort [] = [0]"
            gone <- evalWorkload (workloadOn "/nonexistent/source.hs" src)
            live <- killedWorkload >>= evalWorkload
            show gone `shouldBe` show live

    describe "runWorkloadMode" $ do
        it "writes the classified result to the worker output file" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                wl <- killedWorkload
                let wlPath  = tmp ++ "/workload.json"
                    outPath = tmp ++ "/result.txt"
                writeFile wlPath (encodeWorkload wl)
                runWorkloadMode wlPath (Just outPath)
                content <- readFile outPath
                assertTag "killed" (workerDeserialize (wMutant wl) content)

        it "writes a classified error for a missing workload file" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                let outPath = tmp ++ "/result.txt"
                runWorkloadMode "/nonexistent/mucheck-workload-missing.json" (Just outPath)
                content <- readFile outPath
                case workerDeserialize mutant content of
                    MSumError _ err _ -> err `shouldContain` "worker:"
                    _                 -> expectationFailure "expected MSumError"

        it "cleans up the mutant file when no keep-dir is set" $
            withSystemTempDirectory "mucheck-worker-spec" $ \outer -> do
                -- evalMutant resolves its directory from TMPDIR at run time.
                withEnv "TMPDIR" outer $ do
                    wl <- killedWorkload
                    _ <- evalWorkload wl
                    exists <- doesDirectoryExist (outer ++ "/" ++ hash (_mutant (wMutant wl)))
                    exists `shouldBe` False

        it "keeps the mutant file under the keep-dir" $
            withSystemTempDirectory "mucheck-worker-spec" $ \outer -> do
                let kept = outer ++ "/kept"
                wl <- killedWorkload
                _ <- evalWorkload wl { wKeepMutants = Just kept }
                exists <- doesFileExist
                    (kept ++ "/" ++ hash (_mutant (wMutant wl)) ++ "/Examples/AssertCheckTest.hs")
                exists `shouldBe` True

-- | Run an action with an environment variable set, then unset it.
withEnv :: String -> String -> IO a -> IO a
withEnv name val = bracket_ (setEnv name val) (unsetEnv name)