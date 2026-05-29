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
module Test.Muskell.IntegrationSpec where

import System.Directory (withCurrentDirectory, getCurrentDirectory)
import Test.Hspec

import Test.Muskell (mucheck)
import Test.Muskell.AnalysisSummary (MAnalysisSummary (..))
import Test.Muskell.TestAdapter.AssertCheckAdapter (AssertCheckRun (..))

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
                _maKilled summary `shouldSatisfy` (> 0)
                let total = _maNumMutants summary
                    accounted = _maKilled summary
                              + _maAlive summary
                              + _maErrors summary
                              + _maSkipped summary
                accounted `shouldBe` total
