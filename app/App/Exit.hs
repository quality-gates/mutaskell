-- | Exit-code policy: apply MSI quality gates and escape detection.
module App.Exit
  ( applyExitPolicy
  , isSingleMutantMode
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import System.Exit (ExitCode(..), exitWith)

import App.Opts (Opts(..))
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary(..), summaryCoveredMsi, summaryMsi)

-- | True when --run-mutant-id is set (single-mutant mode skips aggregate output).
isSingleMutantMode :: Opts -> Bool
isSingleMutantMode = isJust . optRunMutantId

-- | Apply MSI quality gates and --fail-on-escaped; exit with the appropriate code on failure.
applyExitPolicy :: Opts -> MAnalysisSummary -> IO ()
applyExitPolicy opts msum = do
  let msi = summaryMsi msum
      coveredMsi = fromMaybe msi (summaryCoveredMsi msum)

  if optIgnoreMsiNoMutations opts && _maNumMutants msum == 0 then return ()
  else do
    case optMinMsi opts of
      Just threshold | msi < threshold -> do
        putStrLn $ "MSI " ++ show msi ++ "% is below threshold " ++ show threshold ++ "%"
        exitWith (ExitFailure 5)
      _ -> return ()

    case optMinCoveredMsi opts of
      Just threshold | coveredMsi < threshold -> do
        putStrLn $ "Covered-MSI " ++ show coveredMsi ++ "% is below threshold " ++ show threshold ++ "%"
        exitWith (ExitFailure 5)
      _ -> return ()

  when (optFailOnEscape opts && _maAlive msum > 0) $ do
    putStrLn $ show (_maAlive msum) ++ " mutant(s) survived; exiting with failure"
    exitWith (ExitFailure 4)
