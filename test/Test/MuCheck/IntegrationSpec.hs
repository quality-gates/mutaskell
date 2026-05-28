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
-- file.
module Test.MuCheck.IntegrationSpec where

import System.Directory (withCurrentDirectory, getCurrentDirectory)
import Test.Hspec

import Test.MuCheck (mucheck)
import Test.MuCheck.AnalysisSummary (MAnalysisSummary (..))
import Test.MuCheck.TestAdapter.AssertCheckAdapter (AssertCheckRun (..))

spec :: Spec
spec = describe "integration" $ do
    it "evaluates AssertCheckTest.hs and kills at least one mutant" $ do
        projDir <- getCurrentDirectory
        result <- withCurrentDirectory projDir $
            mucheck (AssertCheckRun "Examples/AssertCheckTest.hs") ""
        case result of
            Left err ->
                expectationFailure $ "mucheck returned an error: " ++ err
            Right (summary, _mutantSummaries) -> do
                _maNumMutants summary `shouldSatisfy` (> 0)
                -- Include a diagnostic breakdown in the failure message so that
                -- CI logs make the classification visible without needing to
                -- change the assertion semantics.
                let diag = "killed=" ++ show (_maKilled summary)
                         ++ " alive=" ++ show (_maAlive summary)
                         ++ " errors=" ++ show (_maErrors summary)
                         ++ " skipped=" ++ show (_maSkipped summary)
                         ++ " total=" ++ show (_maNumMutants summary)
                if _maKilled summary > 0
                    then return ()
                    else expectationFailure $ "expected at least one kill but got 0 kills: " ++ diag
                let total = _maNumMutants summary
                    accounted = _maKilled summary
                              + _maAlive summary
                              + _maErrors summary
                              + _maSkipped summary
                accounted `shouldBe` total
