-- | End-to-end integration tests for the mucheck evaluation pipeline.
--
-- These tests fork subprocesses via the hint interpreter and are intentionally
-- slow.  Run them selectively with:
--
-- > cabal test --test-option=--match --test-option="/integration/"
--
-- The tests operate on 'Examples/AssertCheckTest.hs' inside a
-- 'withCurrentDirectory' block pointing at the project root so that the hint
-- interpreter can resolve the MuCheck library modules via the GHC environment
-- file written by @cabal build --write-ghc-environment-files=always@.
module Test.Mutaskell.IntegrationSpec where

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import System.Directory
    (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, listDirectory, removeFile,
     withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Trace.Hpc.Mix (BoxLabel (ExpBox), Mix (..), mixCreate)
import Trace.Hpc.Tix (TixModule (..))
import Trace.Hpc.Util (toHpcPos)

import Test.Mutaskell (mucheck)
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary (..))
import Test.Mutaskell.Mutation (genMutants)
import Test.Mutaskell.TestAdapter.AssertCheckAdapter (AssertCheckRun (..))

spec :: Spec
spec = describe "integration" $ do
    it "evaluates AssertCheckTest.hs and kills at least one mutant" $ do
        projDir <- getCurrentDirectory
        -- The hint interpreter resolves the mutaskell library modules at
        -- runtime via the GHC environment file written by
        -- @cabal build --write-ghc-environment-files=always@.  Without it every
        -- mutant fails to load and is recorded as skipped (non-compilable),
        -- which previously surfaced only as an opaque @killed == 0@ failure.
        -- Fail early with an actionable message instead.
        entries <- listDirectory projDir
        unless (any (".ghc.environment." `isPrefixOf`) entries) $
            expectationFailure
                "No .ghc.environment.* file found in the project root. Run \
                \`cabal build --write-ghc-environment-files=always all` before \
                \the test suite so the hint interpreter can resolve the \
                \mutaskell library modules."
        result <- withCurrentDirectory projDir $
            mucheck (AssertCheckRun "Examples/AssertCheckTest.hs") ""
        case result of
            Left err ->
                expectationFailure $ "mucheck returned an error: " ++ err
            Right (summary, _mutantSummaries) -> do
                let total = _maNumMutants summary
                total `shouldSatisfy` (> 0)
                -- Every mutant being skipped as non-compilable means the
                -- interpreter environment is misconfigured, not that the test
                -- suite is weak.  Distinguish the two so a setup problem does
                -- not masquerade as a genuine "nothing killed" result.
                when (_maSkipped summary == total) $
                    expectationFailure
                        "All mutants were skipped as non-compilable. This \
                        \usually means the hint interpreter could not resolve \
                        \the mutaskell library modules — check that \
                        \.ghc.environment.* is present and current."
                _maKilled summary `shouldSatisfy` (> 0)
                let accounted = _maKilled summary
                              + _maAlive summary
                              + _maErrors summary
                              + _maSkipped summary
                accounted `shouldBe` total

    it "samples operators before rendering when the tix holds no coverage" $ do
        projDir <- getCurrentDirectory
        withSystemTempDirectory "mutaskell-tix-empty" $ \tmpDir -> do
            let tixPath = tmpDir </> "empty.tix"
            writeFile tixPath "Tix []"
            result <- withCurrentDirectory projDir $
                mucheck (AssertCheckRun "Examples/AssertCheckTest.hs") tixPath
            case result of
                Left err ->
                    expectationFailure $ "mucheck returned an error: " ++ err
                Right (summary, _mutantSummaries) -> do
                    -- No coverage data means no covered count.
                    _maCoveredNumMutants summary `shouldBe` (-1)
                    _maNumMutants summary `shouldSatisfy` (> 0)
                    _maKilled summary `shouldSatisfy` (> 0)

    it "keeps the full candidate count on the coverage path" $ do
        projDir <- getCurrentDirectory
        withSystemTempDirectory "mutaskell-tix-covered" $ \tmpDir -> do
            let tixPath = tmpDir </> "all-uncovered.tix"
                mixName = "Examples.AssertCheckTest"
                mixPath = ".hpc" </> mixName ++ ".mix"
            -- A tix whose single tick is uncovered, paired with a synthetic
            -- mix entry spanning the whole module, marks every mutant's span
            -- as uncovered while still carrying coverage data.
            writeFile tixPath "Tix [TixModule \"Examples.AssertCheckTest\" 0 1 [0]]"
            withCurrentDirectory projDir $
                bracket (installSyntheticMix mixName) (restoreSyntheticMix mixName) $ \_ -> do
                    result <- mucheck (AssertCheckRun "Examples/AssertCheckTest.hs") tixPath
                    egen <- genMutants "Examples/AssertCheckTest.hs" tixPath
                    case (result, egen) of
                        (Left err, _) ->
                            expectationFailure $ "mucheck returned an error: " ++ err
                        (_, Left err) ->
                            expectationFailure $ "genMutants returned an error: " ++ err
                        (Right (summary, _), Right (genCount, filtered)) -> do
                            -- The covered count matches the full candidate
                            -- population reported by the public generation
                            -- API, not the count surviving the filter.
                            _maCoveredNumMutants summary `shouldBe` genCount
                            -- The coverage filter still applies to what runs.
                            null filtered `shouldBe` True
                            _maNumMutants summary `shouldBe` 0
  where
    -- | Overwrite (and remember) the module's .mix with a synthetic entry
    -- spanning the whole file, so the exact coverage path is exercised without
    -- running hpc.  'restoreSyntheticMix' puts any pre-existing file back.
    installSyntheticMix mixName = do
        now <- getCurrentTime
        createDirectoryIfMissing True ".hpc"
        previous <- doesFileExist mixPath >>= \exists ->
            if exists then Just <$> readFile mixPath else return Nothing
        mixCreate ".hpc" mixName
            (Mix "Examples/AssertCheckTest.hs" now 0 1
                [(toHpcPos (1, 1, 45, 1), ExpBox False)])
        return previous
      where
        mixPath = ".hpc" </> mixName ++ ".mix"

    restoreSyntheticMix mixName previous = case previous of
        Just contents -> writeFile mixPath contents
        Nothing       -> removeFile mixPath
      where
        mixPath = ".hpc" </> mixName ++ ".mix"
