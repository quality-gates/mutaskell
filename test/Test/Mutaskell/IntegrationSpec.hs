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

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import System.Directory (withCurrentDirectory, getCurrentDirectory, listDirectory)
import Test.Hspec

import Test.Mutaskell (mucheck)
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary (..))
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
