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
module Test.MuCheck.IntegrationSpec where

import Data.List (isPrefixOf)
import System.Directory (withCurrentDirectory, getCurrentDirectory, listDirectory)
import Test.Hspec

import Test.MuCheck (mucheck)
import Test.MuCheck.AnalysisSummary (MAnalysisSummary (..))
import Test.MuCheck.Interpreter (MutantSummary (..))
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
                -- Collect diagnostics before asserting so the message shows what happened
                envFiles <- filter (".ghc.environment." `isPrefixOf`) <$> listDirectory projDir
                let diag = "projDir=" ++ projDir
                         ++ " envFiles=" ++ show envFiles
                         ++ " killed=" ++ show (_maKilled summary)
                         ++ " alive=" ++ show (_maAlive summary)
                         ++ " errors=" ++ show (_maErrors summary)
                         ++ " skipped=" ++ show (_maSkipped summary)
                         ++ " total=" ++ show (_maNumMutants summary)
                         ++ case _mutantSummaries of
                              (MSumSkipped _ _:_) -> " (first=skipped)"
                              (MSumError _ e _:_) -> " (first-error=" ++ e ++ ")"
                              (MSumAlive _ _:_)   -> " (first=alive)"
                              _                   -> ""
                if _maKilled summary > 0
                    then return ()
                    else expectationFailure $ "0 kills: " ++ diag
                let total = _maNumMutants summary
                    accounted = _maKilled summary
                              + _maAlive summary
                              + _maErrors summary
                              + _maSkipped summary
                accounted `shouldBe` total
