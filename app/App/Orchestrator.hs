{-# LANGUAGE ScopedTypeVariables #-}

{- | Orchestrator mode: mutation testing that drives the project's /real/ build
and test commands instead of loading mutants into the @hint@ interpreter.

The interpreter path (see "Test.Mutaskell.Interpreter") re-creates a Haskell
environment in-process for one self-contained module at a time.  That cannot
cope with a real multi-package project: it does not see the project's
dependencies, language settings, or test suite.

Orchestrator mode takes the opposite approach, the one used by tools like
Infection (PHP) and Stryker: it edits the source file in place, asks the
project to build and test itself the normal way, and watches whether the tests
notice.  Each mutant is classified by what the real toolchain does with it:

  * build fails        -> SKIPPED   (the compiler rejected it; not a real kill)
  * build ok, test fails -> KILLED  (the test suite detected the change)
  * build ok, test passes -> ALIVE  (a gap in the tests)
  * test times out       -> KILLED  (the change caused non-termination)

Two invariants are upheld deliberately, because violating them silently is
worse than failing loudly:

  * The original file is restored after every mutant /and/ on any exception or
    interrupt, so a crashed or cancelled run never leaves mutated source behind.
  * Mutant generation is forced under a timeout.  Real files (symbol tables,
    large literal lists) can make generation blow up; we abort with an
    actionable message rather than hang.

This module exposes both the single-file entry point ('runOrchestrator', used by
@--exec FILE@) and the reusable building blocks ('evaluateFile', 'classify',
'runCmd', 'restore', 'forceMutantsWithin', 'summarise') that the project-wide
walker in "App.Project" composes to run over a whole repository.
-}
module App.Orchestrator
    ( runOrchestrator
    , Outcome (..)
    , evaluateFile
    , classify
    , runCmd
    , restore
    , summarise
    , execLog
    , stateDir
    , genTimeoutSecs
    ) where

import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (unless, when)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, readFile', stderr)
import System.Process
    ( createProcess
    , interruptProcessGroupOf
    , shell
    , terminateProcess
    , waitForProcess
    , create_group
    )
import System.Timeout (timeout)

import App.Exit (applyExitPolicy)
import App.Filter (applyDisableEnable)
import App.Opts (Opts (..))
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary (..))
import Test.Mutaskell.Config (Config (..), defaultConfig, showMuVar)
import Test.Mutaskell.Mutation (genSampledMutants, getASTFromFile)
import Test.Mutaskell.TestAdapter (Mutant (..))

-- | Outcome of evaluating a single mutant against the real toolchain.
data Outcome = Killed | Alive | Skipped
    deriving (Eq, Show)

-- | Hard ceiling (seconds) on /generation/ for a single file.  The sweep over
-- real repos showed ~1 file in 4 makes generation super-linear; without this
-- the orchestrator would hang exactly where the interpreter path did.
genTimeoutSecs :: Int
genTimeoutSecs = 90

-- | Directory under the project root holding all mutaskell run-state (exec log,
-- progress, survivor report).  Consolidating into one directory keeps the rest
-- of the working tree clean (AC 6) — the user gitignores a single entry, the way
-- Stryker uses @.stryker-tmp@.
stateDir :: FilePath
stateDir = ".mutaskell"

-- | Where each build/test invocation's combined output is written.  Overwritten
-- per command; inspect it to see why a build failed or a test was killed.
execLog :: FilePath
execLog = stateDir ++ "/exec.log"

-- | Entry point for @--exec FILE@ mode (single file).
runOrchestrator :: Opts -> IO ()
runOrchestrator opts = do
    let file     = optFile opts
        buildCmd = fromMaybe "cabal build" (optBuildCmd opts)
        testCmd  = fromMaybe "cabal test"  (optTestCmd opts)
        mtimeout = fmap (* 1000000) (optTimeout opts)

    createDirectoryIfMissing True stateDir

    -- Strict read: we are about to overwrite this file repeatedly, so we must
    -- not hold a lazy read handle open against it.
    origSrc <- readFile' file

    putStrLn $ "Orchestrator mode on " ++ file
    putStrLn $ "  build: " ++ buildCmd
    putStrLn $ "  test:  " ++ testCmd
    putStrLn ""

    -- Baseline: the unmodified project must build and its tests must pass,
    -- otherwise we cannot attribute a test failure to a mutation.
    putStrLn "Baseline: building unmodified source..."
    b0 <- runCmd Nothing buildCmd
    when (b0 /= ExitSuccess) $ abort 3
        "Baseline build failed on unmodified source. Fix the build before running mutation testing."
    putStrLn "Baseline: running tests on unmodified source..."
    t0 <- runCmd mtimeout testCmd
    when (t0 /= ExitSuccess) $ abort 3
        "Baseline tests failed (or timed out) on unmodified source. The suite must be green to start."
    putStrLn "Baseline OK.\n"

    -- Generate (operators sampled before rendering) under a timeout to defend
    -- against generation blow-up.  getASTFromFile uses CPP-aware parsing so
    -- #if/#ifdef files still generate.
    ast <- either (abort 2 . ("Parse error: " ++)) return =<< getASTFromFile file
    let maxN = fromMaybe (maxNumMutants defaultConfig) (optMaxMutants opts)
        cfg  = defaultConfig { maxNumMutants = maxN }
    forced <- timeout (genTimeoutSecs * 1000000) $ do
        ms <- genSampledMutants cfg ast
        let filtered = applyDisableEnable (optDisable opts) (optEnable opts) ms
        _ <- evaluate (sum (map (length . _mutant) filtered))
        return filtered
    mutants <- case forced of
        Just ms -> return ms
        Nothing -> abort 6 $
            "Mutant generation exceeded " ++ show genTimeoutSecs
            ++ "s on this file (dense literals/tables are the usual cause). "
            ++ "Narrow scope with --max-mutants, --enable, or coverage."

    let total = length mutants
    when (total == 0) $ do
        putStrLn "No mutants generated for this file."
        exitSuccess
    putStrLn $ "Evaluating " ++ show total ++ " mutant(s) against the real build...\n"

    -- The original is restored after every mutant and, crucially, in `finally`
    -- so an exception or Ctrl-C cannot leave the working tree mutated.
    results <- evaluateFile file buildCmd testCmd mtimeout Nothing file origSrc mutants
        `finally` restore file origSrc

    let msum = summarise results
    printResults origSrc results
    print msum
    applyExitPolicy opts msum

-- | Evaluate every mutant for one file in turn, restoring the original after
-- each one.  @label@ prefixes the per-mutant progress line (the file path, when
-- driven by the project walker).  If a @deadline@ is supplied and passes, the
-- remaining mutants are left unevaluated and only those done so far returned —
-- this is how the wall-clock budget (AC 15) is honoured mid-file.
evaluateFile
    :: String            -- ^ progress label (e.g. the file path)
    -> String            -- ^ build command
    -> String            -- ^ test command
    -> Maybe Int         -- ^ per-mutant test timeout (microseconds)
    -> Maybe UTCTime     -- ^ optional run deadline
    -> FilePath          -- ^ file to mutate in place
    -> String            -- ^ original source (restored after each mutant)
    -> [Mutant]
    -> IO [(Mutant, Outcome)]
evaluateFile label buildCmd testCmd mtimeout deadline file origSrc mutants =
    go (zip [1 :: Int ..] mutants)
  where
    n = length mutants
    go [] = return []
    go ((i, m) : rest) = do
        past <- pastDeadline deadline
        if past
            then return []
            else do
                outcome <- runOne i m
                ((m, outcome) :) <$> go rest
    runOne i m = do
        writeFile file (_mutant m)
        outcome <- classify buildCmd testCmd mtimeout
            `finally` restore file origSrc
        hPutStrLn stderr $
            "[" ++ show i ++ "/" ++ show n ++ "] " ++ label ++ "  "
            ++ padTo 16 (showMuVar (_mtype m)) ++ "  " ++ show outcome
        return outcome

-- | Has the optional deadline passed?
pastDeadline :: Maybe UTCTime -> IO Bool
pastDeadline Nothing   = return False
pastDeadline (Just dl) = (> dl) <$> getCurrentTime

-- | Build, then (if it builds) test, mapping toolchain results to outcomes.
classify :: String -> String -> Maybe Int -> IO Outcome
classify buildCmd testCmd mtimeout = do
    bc <- runCmd Nothing buildCmd
    if bc /= ExitSuccess
        then return Skipped
        else do
            tc <- runCmdTimeoutAware mtimeout testCmd
            return $ case tc of
                TimedOut       -> Killed
                Exited ExitSuccess -> Alive
                Exited _       -> Killed

-- | Result of a command that may have been killed for exceeding its timeout.
data CmdResult = Exited ExitCode | TimedOut

-- | Run a shell command, returning its exit code (no timeout distinction).
runCmd :: Maybe Int -> String -> IO ExitCode
runCmd mt cmd = do
    r <- runCmdTimeoutAware mt cmd
    return $ case r of
        Exited ec -> ec
        TimedOut  -> ExitFailure 124

-- | Run a shell command in its own process group, killing the whole group if it
-- exceeds the timeout (so cabal/ghc children do not linger).
runCmdTimeoutAware :: Maybe Int -> String -> IO CmdResult
runCmdTimeoutAware mt cmd = do
    -- Redirect the command's output to a log file via the shell.  We must not
    -- use CreatePipe here without draining it: cabal/ghc produce enough output
    -- to fill the pipe buffer and deadlock the child.
    let wrapped = "( " ++ cmd ++ " ) > " ++ execLog ++ " 2>&1"
    (_, _, _, ph) <- createProcess (shell wrapped) { create_group = True }
    case mt of
        Nothing -> Exited <$> waitForProcess ph
        Just us -> do
            m <- timeout us (waitForProcess ph)
            case m of
                Just ec -> return (Exited ec)
                Nothing -> do
                    _ <- (try (interruptProcessGroupOf ph) :: IO (Either SomeException ()))
                    terminateProcess ph
                    _ <- waitForProcess ph
                    return TimedOut

-- | Restore the original file contents.
restore :: FilePath -> String -> IO ()
restore file orig = writeFile file orig

-- | Build an analysis summary from orchestrator outcomes.
summarise :: [(Mutant, Outcome)] -> MAnalysisSummary
summarise rs = MAnalysisSummary
    { _maCoveredNumMutants = -1
    , _maNumMutants        = length rs
    , _maAlive             = count Alive
    , _maKilled            = count Killed
    , _maErrors            = 0
    , _maSkipped           = count Skipped
    }
  where count o = length (filter ((== o) . snd) rs)

-- | Print surviving mutants with the line they changed (the actionable output).
printResults :: String -> [(Mutant, Outcome)] -> IO ()
printResults origSrc rs = do
    let alive = [m | (m, Alive) <- rs]
    unless (null alive) $ do
        putStrLn $ "\nSurviving mutants (" ++ show (length alive) ++ "):"
        mapM_ (printSurvivor origSrc) alive
    putStrLn ""

-- | Show one surviving mutant as a before/after of its first changed line.
printSurvivor :: String -> Mutant -> IO ()
printSurvivor origSrc m =
    case firstDiff origSrc (_mutant m) of
        Just (ln, a, b) -> do
            putStrLn $ "  " ++ showMuVar (_mtype m) ++ " @ line " ++ show ln
            putStrLn $ "    - " ++ a
            putStrLn $ "    + " ++ b
        Nothing -> putStrLn $ "  " ++ showMuVar (_mtype m) ++ " (no line diff)"

-- | First line that differs between original and mutant (1-indexed).
firstDiff :: String -> String -> Maybe (Int, String, String)
firstDiff a b =
    find (\(_, x, y) -> x /= y)
        (zip3 [1 ..] (lines a ++ repeat "") (lines b ++ repeat ""))

-- | Pad a string with trailing spaces to a given width.
padTo :: Int -> String -> String
padTo n s = s ++ replicate (max 0 (n - length s)) ' '

-- | Print a message to stderr and exit with the given code.
abort :: Int -> String -> IO a
abort code msg = do
    hPutStrLn stderr msg
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)
