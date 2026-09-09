-- Throwaway benchmark for the issue #36 session-reuse prototype.  Not part of
-- the cabal file: compile it directly like bench/sample.sh does.
--
-- Generates a mutant module with N ordered 'IO Bool' tests (all passing, or
-- failing on the last one), runs it under both session policies and reports
-- sessions, loads, tests run, setup versus test time, wall time and peak RTS
-- residency.  Results are forced so laziness cannot hide the work.
module Main where

import Control.Exception (evaluate)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)
import Trace.Hpc.Util (toHpcPos)

import qualified Test.Mutaskell.Interpreter.ReusedSession as RS
import Test.Mutaskell.Config (MuVar (..))
import Test.Mutaskell.TestAdapter

-- Same catch-free adapter the equivalence tests use: 'IO Bool' results with
-- no 'SomeException' handler between the test and the summary.
instance Summarizable Bool where
    testSummary _ _ result = Summary (_ioLog result)
    isSuccess = id
    isFailure = not
    isOther _ = False

mutantSrc :: Int -> Bool -> String
mutantSrc n lateKill = unlines $
    ["module BenchMutant where"]
        ++ concat
            [ [ printf "test%d :: IO Bool" i
              , printf "test%d = pure %s" i (if lateKill && i == n then "False" else "True")
              , ""
              ]
            | i <- [1 .. n]
            ]

testNames :: Int -> [String]
testNames n = [printf "test%d" i | i <- [1 .. n]]

runPolicy ::
    Int -> Bool -> FilePath -> RS.PkgEnvCache -> RS.SessionPolicy ->
    IO (RS.ReusedStats, [InterpreterOutput Bool])
runPolicy n lateKill dir pkgEnv policy =
    RS.evalMutantWithPolicy
        Nothing
        True
        dir
        pkgEnv
        []
        policy
        (testNames n)
        (toMutant (MutateValues, toHpcPos (1, 1, 1, 1), mutantSrc n lateKill))

main :: IO ()
main = do
    [mode, size, policyArg, trial] <- getArgs
    let n = read size :: Int
        lateKill = mode == "late-kill"
    statsEnabled <- getRTSStatsEnabled
    if not statsEnabled
        then error "run with -T (the wrapper script passes -with-rtsopts=-T)"
        else pure ()
    pkgEnv <- RS.detectPkgEnv
    wall0 <- getCurrentTime
    (stats, outs) <- withSystemTempDirectory "mutbench" $ \dir ->
        runPolicy n lateKill dir pkgEnv (policyOf policyArg)
    wall1 <- getCurrentTime
    -- Force every result: classification must not be left unevaluated.
    mapM_ (evaluate . _io) outs
    endStats <- getRTSStats
    printf
        "%s n=%d %s trial=%s sessions=%d loads=%d tests_run=%d setup_ms=%.1f test_ms=%.1f wall_ms=%.1f outcomes=%s peak_rts_mib=%.1f\n"
        mode
        n
        policyArg
        trial
        (RS.rsSessions stats)
        (RS.rsLoads stats)
        (RS.rsTestsRun stats)
        (fromInteger (RS.rsSetupNs stats) / 1e6 :: Double)
        (fromInteger (RS.rsTestNs stats) / 1e6 :: Double)
        (realToFrac (diffUTCTime wall1 wall0) * 1000 :: Double)
        (show (length outs))
        (fromIntegral (max_mem_in_use_bytes endStats) / 1048576 :: Double)
  where
    policyOf m = case m of
        "fresh"   -> RS.FreshPerTest
        "reused"  -> RS.LoadOncePerMutant
        _         -> error "policy must be fresh or reused"
