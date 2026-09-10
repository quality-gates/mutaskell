-- | Tests for the worker workload transport: the JSON document a parent
-- process hands to each interpreter worker child so the child can evaluate
-- the parent's already-selected mutant without regenerating candidates.
module Test.Mutaskell.WorkerSpec where

import Control.Exception (bracket_)
import Control.Monad (filterM, forM, unless)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Timeout (timeout)
import Data.List (maximumBy)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Ord (comparing)
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe, withObject, (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import System.Directory
    (doesDirectoryExist, doesFileExist, getCurrentDirectory, getModificationTime, listDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
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

-- | Replace a source line in the example module, producing source text for a
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

-- | A workload whose mutant source carries non-ASCII text (a comment
-- annotating the mutated line).  The transport must preserve it byte for byte,
-- and the mutation itself stays detected by the tests.
nonAsciiWorkload :: IO Workload
nonAsciiWorkload =
    workloadOn "Examples/AssertCheckTest.hs"
        <$> editedExample "qsort [] = []" "qsort [] = [0] -- mutated: π < 3,15"

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
workloadDoc :: [(String, A.Value)] -> BL.ByteString
workloadDoc overrides = A.encode obj
  where
    obj = foldl (\m (k, v) -> KM.insert (Key.fromString k) v m) baseKm overrides
    baseKm = case A.decode (encodeWorkload workload) of
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

-- | Locate the built mutaskell binary so subprocess tests can spawn real
-- worker children. dist-newstyle accumulates binaries from past versions, so
-- the most recently built one wins. Override with MUCHECK_BIN.
findMucheckBin :: IO (Maybe FilePath)
findMucheckBin = do
    env <- lookupEnv "MUCHECK_BIN"
    case env of
        Just p  -> return (Just p)
        Nothing -> do
            root <- getCurrentDirectory
            let dist = root ++ "/dist-newstyle"
            exists <- doesDirectoryExist dist
            if not exists then return Nothing else newestBin dist (12 :: Int)
  where
    newestBin dir depth = do
        entries <- listDirectory dir
        let candidates = [dir ++ "/" ++ e | e <- entries, e == "mutaskell"]
        files <- filterM doesFileExist candidates
        subresults <- if depth <= 1 then return [] else do
            subdirs <- filterM (\e -> doesDirectoryExist (dir ++ "/" ++ e)) entries
            mapM (\d -> newestBin (dir ++ "/" ++ d) (depth - 1)) subdirs
        let found = files ++ catMaybes subresults
        case found of
            [] -> return Nothing
            _  -> Just . snd . maximum <$> mapM (\f -> (,) <$> getModificationTime f <*> pure f) found

runMucheck :: FilePath -> [String] -> IO (ExitCode, String)
runMucheck bin args = do
    (ec, out, errOut) <- readProcessWithExitCode bin args ""
    return (ec, out ++ errOut)

-- | Run a child and fail with its output if it did not exit cleanly.
expectCleanRun :: FilePath -> [String] -> IO ()
expectCleanRun bin args = do
    (ec, output) <- runMucheck bin args
    unless' ec output
  where
    unless' ec output
        | ec == ExitSuccess = return ()
        | otherwise = expectationFailure
            ("child exited " ++ show ec ++ ": " ++ output)

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

        it "round-trips non-ASCII mutant source without corrupting it" $
            nonAsciiWorkload >>= \wl ->
                decodeWorkload (encodeWorkload wl) `shouldBe` Right wl

        it "round-trips a workload without timeout or keep-dir" $ do
            let bare = workload { wTimeout = Nothing, wKeepMutants = Nothing, wTestArgs = [] }
            decodeWorkload (encodeWorkload bare) `shouldBe` Right bare

        it "rejects malformed JSON with a classified error" $
            classifyError (decodeWorkload (fromString "not json at all"))

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
                BL.writeFile path (encodeWorkload workload)
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

        it "honours the transported per-mutant timeout" $ do
            -- A mutant whose first test diverges, under a one-second timeout.
            -- The AssertCheck adapter catches the async Timeout exception and
            -- reports a failed test, so the assertion is that the run is cut
            -- short at all (unbounded divergence would hit the outer guard).
            -- A microsecond timeout is not safe to test with: it can interrupt
            -- GHC API initialisation and abort the whole spec process.
            src <- editedExample "test_sortEmpty = assertCheck $ null (qsort [])"
                                 "test_sortEmpty = assertCheck $ qsort [1 ..] == [1 ..]"
            let wl = (workloadOn "Examples/AssertCheckTest.hs" src) { wTimeout = Just 1000000 }
            t0 <- getCurrentTime
            msum' <- timeout 60000000 (evalWorkload wl)
            t1 <- getCurrentTime
            case msum' of
                Nothing -> expectationFailure "evaluation did not finish; timeout not honoured"
                Just _  -> diffUTCTime t1 t0 `shouldSatisfy` (< 30)

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
                BL.writeFile wlPath (encodeWorkload wl)
                runWorkloadMode wlPath (Just outPath)
                content <- BL.readFile outPath
                assertTag "killed" (workerDeserialize (wMutant wl) content)

        it "preserves non-ASCII mutant source through the file round trip" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                wl <- nonAsciiWorkload
                let wlPath  = tmp ++ "/workload.json"
                    outPath = tmp ++ "/result.txt"
                BL.writeFile wlPath (encodeWorkload wl)
                runWorkloadMode wlPath (Just outPath)
                content <- BL.readFile outPath
                assertTag "killed" (workerDeserialize (wMutant wl) content)

        it "writes a classified error for a missing workload file" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                let outPath = tmp ++ "/result.txt"
                runWorkloadMode "/nonexistent/mucheck-workload-missing.json" (Just outPath)
                content <- BL.readFile outPath
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

    describe "worker subprocess" $ do
        it "links the mutaskell executable against the threaded runtime" $ do
            -- runWithWorkers fans out via forkIO and waits on each child with
            -- waitForProcess. On the non-threaded RTS that wait blocks the
            -- whole runtime, so --workers N evaluates one child at a time
            -- (issue #59); the threaded RTS is what lets the parent wait on
            -- many children concurrently.
            bin <- findMucheckBin
            case bin of
                Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                Just exe -> do
                    (ec, out, _) <- readProcessWithExitCode exe ["+RTS", "--info"] ""
                    ec `shouldBe` ExitSuccess
                    out `shouldContain` "\"RTS way\", \"rts_thr\""

        it "dispatches a workload through the CLI to a fresh child process" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                bin <- findMucheckBin
                case bin of
                    Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                    Just exe -> do
                        wl <- killedWorkload
                        let wlPath  = tmp ++ "/workload.json"
                            outPath = tmp ++ "/result.txt"
                        BL.writeFile wlPath (encodeWorkload wl)
                        expectCleanRun exe
                            [ wTarget wl, "--run-mutant-workload", wlPath
                            , "--worker-output", outPath ]
                        content <- BL.readFile outPath
                        assertTag "killed" (workerDeserialize (wMutant wl) content)

        it "preserves non-ASCII mutant source on the way to a child process" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                -- The parent writes the document and the child reads raw
                -- bytes, so the mutant text is classified identically to the
                -- same workload evaluated in-process.
                bin <- findMucheckBin
                case bin of
                    Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                    Just exe -> do
                        wl <- nonAsciiWorkload
                        let wlPath  = tmp ++ "/workload.json"
                            outPath = tmp ++ "/result.txt"
                        BL.writeFile wlPath (encodeWorkload wl)
                        expectCleanRun exe
                            [ wTarget wl, "--run-mutant-workload", wlPath
                            , "--worker-output", outPath ]
                        content <- BL.readFile outPath
                        assertTag "killed" (workerDeserialize (wMutant wl) content)

        it "evaluates the workload without regenerating from the target source" $
            withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                -- The target is a copy that cannot be parsed back into the
                -- mutant, so any child that regenerated candidates would fail
                -- with a parse error instead of producing a classified result.
                bin <- findMucheckBin
                case bin of
                    Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                    Just exe -> do
                        let target = tmp ++ "/AssertCheckTest.hs"
                        writeFile target "@@@ not haskell @@@"
                        src <- editedExample "qsort [] = []" "qsort [] = [0]"
                        let wl = workloadOn target src
                            wlPath  = tmp ++ "/workload.json"
                            outPath = tmp ++ "/result.txt"
                        BL.writeFile wlPath (encodeWorkload wl)
                        expectCleanRun exe
                            [ target, "--run-mutant-workload", wlPath
                            , "--worker-output", outPath ]
                        content <- BL.readFile outPath
                        assertTag "killed" (workerDeserialize (wMutant wl) content)

        it "workers 1, 2 and 4 agree on outcomes for the same workload" $ do
            bin <- findMucheckBin
            case bin of
                Nothing -> pendingWith "mucheck binary not built (run cabal build all)"
                Just exe -> do
                    summaries <- forM [1, 2, 4 :: Int] $ \n ->
                        withSystemTempDirectory "mucheck-worker-spec" $ \tmp -> do
                            let outPath = tmp ++ "/summary.json"
                            (ec, _) <- runMucheck exe
                                [ "Examples/AssertCheckTest.hs", "--workers", show n
                                , "--logger-json", outPath ]
                            ec `shouldBe` ExitSuccess
                            parsed <- A.eitherDecodeFileStrict' outPath
                            json <- either (fail . ("bad JSON summary: " ++)) return parsed
                            total <- jsonKey "total" json
                            killed <- jsonKey "killed" json
                            alive <- jsonKey "alive" json
                            skipped <- jsonKey "skipped" json
                            errors <- jsonKey "errors" json
                            return (total, killed, alive, skipped, errors)
                    let (firstRun:rest) = summaries
                    mapM_ (shouldBe firstRun) rest

-- | Read an integer field from a parsed logger summary.
jsonKey :: String -> A.Value -> IO Int
jsonKey k v = case parseMaybe (withObject "summary" (.: Key.fromString k)) v of
    Just n  -> return n
    Nothing -> fail ("missing key in JSON summary: " ++ k)

-- | Run an action with an environment variable set, then unset it.
withEnv :: String -> String -> IO a -> IO a
withEnv name val = bracket_ (setEnv name val) (unsetEnv name)