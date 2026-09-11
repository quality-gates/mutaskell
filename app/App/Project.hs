{-# LANGUAGE ScopedTypeVariables #-}

{- | Project mode: run mutation testing over a whole repository the way
Infection (PHP) or a folder-level Go tool does — point it at a directory, and it
drives the project's own build and test commands across every source file it can
find.

This is the orchestrator (see "App.Orchestrator") lifted from one file to a
tree.  The design decisions that make it usable on real repos:

  * /Auto-detection/ (AC 2): with no @--build-cmd@/@--test-cmd@, a cabal project
    is driven with @cabal build all@ / @cabal test all@ and a stack project with
    @stack build@ / @stack test@.  The user never has to supply commands.
  * /Discovery/ (AC 3): source files come from the @hs-source-dirs@ declared in
    the project's @.cabal@ files, plus each package's own directory (so a library
    stanza that omits @hs-source-dirs@ — defaulting to the package dir — is still
    found).
  * /Resilience/ (AC 4): a file that fails to parse or whose generation blows up
    is logged and skipped; the run continues and reports the skip.
  * /Aggregate score/ (AC 5): the run ends with one project-level summary.
  * /Restore/ (AC 6): the working tree is restored after every mutant and on any
    interrupt, so @git status@ is clean afterwards.
  * /Resumable/ (AC 9): completed files are recorded; a re-run skips them, and
    surviving mutants are written to a report file.
  * /Budget/ (AC 15): @--max-mutants@ caps the total mutants evaluated and
    @--time-budget@ caps wall-clock; either way the run stops early and reports a
    partial score rather than running unbounded.
-}
module App.Project
    ( DiscoveryStats (..)
    , clearProgress
    , discoverSources
    , discoverSourcesWithStats
    , distribute
    , findProjectRoot
    , isProjectRoot
    , progressFile
    , readProgress
    , recordDone
    , restrictToShard
    , runProject
    , runProjectDryRun
    ) where

import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (filterM, foldM, forM, unless, when)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf, isSuffixOf, nub)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    , removeFile
    , setCurrentDirectory
    )
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (makeRelative, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, readFile', stderr)
import System.Process (callProcess, createProcess, proc, waitForProcess)

import App.Exit (applyExitPolicy)
import App.Filter (applyDisableEnable)
import App.Opts (Opts (..), missingCoverageForMinCoveredMsi)
import App.Orchestrator
    ( Outcome (..)
    , evaluateFile
    , execLog
    , runCmd
    , restore
    , stateDir
    , summarise
    )
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary (..), forceSummary)
import Test.Mutaskell.Config (Config (..), defaultConfig, showMuVar)
import Test.Mutaskell.Mutation
    ( genSampledMutantsGated
    , getASTFromFile
    , getModuleName
    , readCabalMacroScans
    )
import Test.Mutaskell.Tix
    ( Span
    , TixIndex
    , getUnCoveredPatchesFromIndex
    , parseTixIndex
    )
import Test.Mutaskell.TestAdapter (Mutant (..))

-- | File recording fully-completed source files, for resume (AC 9).
progressFile :: FilePath
progressFile = stateDir ++ "/progress"

-- | Human-readable report of surviving mutants (AC 9).
survivorsFile :: FilePath
survivorsFile = stateDir ++ "/survivors.txt"

-- | Run mutation testing across a whole project directory.  With @--jobs N@ (and
-- when this is not itself a worker) the work is split across N isolated copies of
-- the repo (see 'runParallel'); otherwise it runs in this process.
runProject :: Opts -> IO ()
runProject opts
    | optJobs opts > 1 && isNothing (optOnlyFiles opts) = runParallel opts
    | otherwise                                        = runSerial opts

-- | Single-process project run (also used as each parallel worker, restricted to
-- its shard via @--only-files@).
runSerial :: Opts -> IO ()
runSerial opts = do
    scope <- canonicalizePath (optFile opts)
    root <- maybe (noProjectRootFound scope) return =<< findProjectRoot scope
    setCurrentDirectory root
    createDirectoryIfMissing True stateDir
    (buildCmd, testCmd) <- detectCommands opts
    let mtimeout = fmap (* 1000000) (optTimeout opts)
        relScope = normalise (makeRelative root scope)

    putStrLn $ "Project mode on " ++ scope
    putStrLn $ "  root:  " ++ root
    putStrLn $ "  build: " ++ buildCmd
    putStrLn $ "  test:  " ++ testCmd
    reportBudget opts
    putStrLn ""

    allFiles <- discoverSources opts { optFile = relScope }
    files <- restrictToShard opts allFiles
    when (null files) $ do
        hPutStrLn stderr "No Haskell source files discovered. Nothing to do."
        maybe (return ()) (`writeFile` "0 0 0 0") (optResultOut opts)
        exitSuccess

    done <- readProgress
    let pending = dropCompleted done files
    if null pending
        then do
            putStrLn $ "All " ++ show (length files)
                ++ " discovered file(s) already done (per "
                ++ progressFile ++ "). Nothing to do."
            maybe (return ()) (`writeFile` "0 0 0 0") (optResultOut opts)
        else do
            putStrLn $ "Discovered " ++ show (length files) ++ " source file(s); "
                ++ show (length done) ++ " already done, "
                ++ show (length pending) ++ " pending.\n"

            -- Load coverage before the baseline so an unreadable --tix fails fast.
            coverage <- loadCoverage opts

            -- Baseline runs exactly once for the whole project (AC 10), not per file.
            putStrLn "Baseline: building unmodified project..."
            b0 <- runCmd Nothing buildCmd
            when (b0 /= ExitSuccess) $ baselineBuildFailed buildCmd
            putStrLn "Baseline: running the test suite on unmodified project..."
            t0 <- runCmd mtimeout testCmd
            when (t0 /= ExitSuccess) $ baselineTestFailed testCmd (t0 == ExitFailure 124)
            putStrLn "Baseline OK.\n"

            -- Budget state shared across files.
            start <- getCurrentTime
            let deadline = fmap (\s -> addUTCTime (fromIntegral s) start) (optTimeBudget opts)
                totalBudget = fromMaybe maxBound (optMaxMutants opts)
            budgetRef <- newIORef totalBudget

            -- Each file's evaluation restores the original after every mutant and in a
            -- `finally`, so an interrupt cannot leave mutated source behind (AC 6).
            -- Between files no file is in a mutated state, so no extra guard is needed.
            (completedAll, msum) <- walk opts buildCmd testCmd mtimeout deadline coverage budgetRef pending

            putStrLn ""
            putStrLn "==== Project mutation summary ===="
            print msum
            putStrLn $ "Surviving mutants written to " ++ survivorsFile
                ++ " (when any survived)."
            writeResult opts msum
            doneAfter <- readProgress
            let allDone = completedAll && null (dropCompleted doneAfter files)
            when (allDone && isNothing (optOnlyFiles opts)) clearProgress
            applyExitPolicy opts msum

-- | Walk pending files, folding each file's strict summary into the project
-- total and honouring the budget.  A file's full results are released once its
-- survivor report is written, so mutant source bodies do not stay live for the
-- rest of the run — summary state stays O(1) per file, not O(file source).
-- Returns @(completedAll, summary)@ where @completedAll@ is 'True' if all
-- pending files were evaluated without stopping early for a budget.
walk
    :: Opts -> String -> String -> Maybe Int -> Maybe UTCTime
    -> CoverageSnapshot -> IORef Int -> [FilePath] -> IO (Bool, MAnalysisSummary)
walk opts buildCmd testCmd mtimeout deadline coverage budgetRef pending = do
    sumRef <- newIORef mempty
    let go []       = return True
        go (f : fs) = do
            stop <- shouldStop deadline budgetRef
            if stop
                then do
                    hPutStrLn stderr "Budget exhausted; stopping with a partial result."
                    return False
                else do
                    fsum <- processFile opts buildCmd testCmd mtimeout deadline
                            coverage budgetRef f
                    modifyIORef' sumRef (<> fsum)
                    go fs
    completedAll <- go pending
    total <- readIORef sumRef
    _ <- evaluate (forceSummary total)
    return (completedAll, total)

-- | True if the time budget has passed or the mutant budget is spent.
shouldStop :: Maybe UTCTime -> IORef Int -> IO Bool
shouldStop deadline budgetRef = do
    remaining <- readIORef budgetRef
    if remaining <= 0
        then return True
        else case deadline of
            Nothing -> return False
            Just dl -> (>= dl) <$> getCurrentTime

-- | Process one source file: parse, generate (bounded), sample, evaluate.
-- Returns the file's strict summary; any failure is logged and the file
-- skipped, so the run survives bad files (AC 4).  The full per-mutant results
-- never leave this function: survivors are reported from them here, then the
-- results are dropped.
processFile
    :: Opts -> String -> String -> Maybe Int -> Maybe UTCTime
    -> CoverageSnapshot -> IORef Int -> FilePath -> IO MAnalysisSummary
processFile opts buildCmd testCmd mtimeout deadline coverage budgetRef file = do
    e <- try (processFile' opts buildCmd testCmd mtimeout deadline coverage budgetRef file)
    case e of
        Right fsum -> return fsum
        Left (ex :: SomeException) -> do
            hPutStrLn stderr $ "SKIP " ++ file ++ ": " ++ show ex
            recordDone file
            return mempty

processFile'
    :: Opts -> String -> String -> Maybe Int -> Maybe UTCTime
    -> CoverageSnapshot -> IORef Int -> FilePath -> IO MAnalysisSummary
processFile' opts buildCmd testCmd mtimeout deadline coverage budgetRef file = do
    origSrc <- readFile' file
    eAst <- getASTFromFile file
    case eAst of
        Left err -> do
            hPutStrLn stderr $ "SKIP " ++ file ++ " (parse): " ++ firstLine err
            recordDone file
            return mempty
        Right ast -> do
            remaining <- readIORef budgetRef
            let perFileCap = min remaining (maxNumMutants defaultConfig)
                cfg        = defaultConfig { maxNumMutants = perFileCap }
            muncov <- resolveUncovered coverage (getModuleName ast)
            (genComplete, sampled) <- genWithinBudget genBudgetSecs $ do
                ms <- genSampledMutantsGated cfg muncov ast
                return (applyDisableEnable (optDisable opts) (optEnable opts) ms)
            if null sampled
                then do
                    -- Record done only if generation genuinely finished (zero
                    -- mutants), not if it was time-truncated — otherwise a slow
                    -- file is silently skipped forever on resume.
                    when genComplete (recordDone file)
                    return mempty
                else do
                    hPutStrLn stderr $ "FILE " ++ file ++ ": "
                        ++ show (length sampled) ++ " mutant(s)"
                    rs <- evaluateFile file buildCmd testCmd mtimeout deadline
                            file origSrc sampled
                        `finally` restore file origSrc
                    modifyIORef' budgetRef (subtract (length rs))
                    recordSurvivors origSrc file rs
                    -- Record done only if generation finished and we evaluated
                    -- the whole file (a budget cut mid-file leaves it for resume).
                    when (genComplete && length rs == length sampled) (recordDone file)
                    -- Fold strict counters while `rs` is in scope, then let it
                    -- go: the summary must not drag this file's mutant sources
                    -- through the rest of the run.
                    return $! forceSummary (summarise rs)

-- | Dry run over a project: discover files and report per-file generation counts
-- without building or testing.  Cheap way to verify discovery + bounded
-- generation (AC 3, AC 13) on a repo that is not built.
runProjectDryRun :: Opts -> IO ()
runProjectDryRun opts = do
    scope <- canonicalizePath (optFile opts)
    root <- maybe (noProjectRootFound scope) return =<< findProjectRoot scope
    setCurrentDirectory root
    let relScope = normalise (makeRelative root scope)
    (files, stats) <- discoverSourcesWithStats opts { optFile = relScope }
    putStrLn $ "Project dry-run on " ++ scope
    putStrLn $ "  root:  " ++ root
    putStrLn $ "Discovered " ++ show (length files) ++ " source file(s)."
    putStrLn $ "Discovery scan: " ++ show (dsRoots stats) ++ " root(s) walked, "
        ++ show (dsDirs stats) ++ " director(ies) listed.\n"
    coverage <- if null files then return Nothing else loadCoverage opts
    total <- foldM (countFile opts coverage) 0 files
    macroScans <- readCabalMacroScans
    putStrLn $ "\nCPP macro scans: " ++ show macroScans
    putStrLn $ "Total generated mutants (sampled per file): " ++ show total

countFile :: Opts -> CoverageSnapshot -> Int -> FilePath -> IO Int
countFile opts coverage acc file = do
    e <- try (dryCount opts coverage file) :: IO (Either SomeException (Maybe Int))
    case e of
        Left ex -> do
            hPutStrLn stderr $ "SKIP " ++ file ++ ": " ++ firstLine (show ex)
            return acc
        Right Nothing -> do
            hPutStrLn stderr $ "SKIP " ++ file
            return acc
        Right (Just n) -> do
            putStrLn $ "  " ++ file ++ "  " ++ show n
            return (acc + n)

dryCount :: Opts -> CoverageSnapshot -> FilePath -> IO (Maybe Int)
dryCount opts coverage file = do
    eAst <- getASTFromFile file
    case eAst of
        Left _    -> return Nothing
        Right ast -> do
            let cap = fromMaybe (maxNumMutants defaultConfig) (optMaxMutants opts)
                cfg = defaultConfig { maxNumMutants = cap }
            muncov <- resolveUncovered coverage (getModuleName ast)
            -- A dry run reports what generation actually produces, bounded only by
            -- a generous timeout (not the per-file render budget used by real runs);
            -- a file that needs longer than that to fully generate is reported as a
            -- skip (e.g. very large modules — see notes in genWithinBudget).
            timeout (genSetupCeilingSecs * 1000000) $ do
                ms <- genSampledMutantsGated cfg muncov ast
                let ms' = applyDisableEnable (optDisable opts) (optEnable opts) ms
                evaluate (length ms')

-- | Soft per-file generation budget (seconds).  Generation is bounded so no
-- single file dominates the run: the operator-level sampling caps the candidate
-- count, but applying and rendering each mutant is a full-AST traversal, so a
-- huge module can still be slow.  We render mutants until this budget elapses
-- and proceed with however many we have (AC 13) rather than skipping the file.
genBudgetSecs :: Int
genBudgetSecs = 5

-- | Generate mutants in two separately-bounded phases and return
-- @(completed, mutants)@.  @completed@ is 'False' if either phase was cut short,
-- so the caller avoids recording a time-truncated file as fully done (which would
-- skip it forever on resume).
--
-- Phase 1 (setup): operator selection + sampling.  On a large module this is
-- several whole-AST traversals and can take many seconds; it is bounded by
-- 'genSetupCeilingSecs'.  Crucially the render budget does /not/ start until this
-- finishes — otherwise setup eats the whole budget and the file yields zero
-- mutants (which is exactly what happened to mutaskell's own @Mutation.hs@).
--
-- Phase 2 (render): force mutants one at a time into an accumulator until @secs@
-- elapses, so a module with many mutants still completes promptly with a
-- representative sample.  Accumulating into an 'IORef' means a hard-timeout (a
-- single pathological render that overruns the soft deadline) still yields the
-- mutants rendered so far instead of discarding everything.
genWithinBudget :: Int -> IO [Mutant] -> IO (Bool, [Mutant])
genWithinBudget secs act = do
    mlist <- timeout (genSetupCeilingSecs * 1000000) act
    case mlist of
        Nothing -> return (False, [])   -- setup itself ran away
        Just ms -> do
            acc <- newIORef []
            deadline <- addUTCTime (fromIntegral secs) <$> getCurrentTime
            done <- timeout (4 * secs * 1000000) (forceInto acc deadline ms)
            got  <- reverse <$> readIORef acc
            return (done == Just True, got)
  where
    forceInto acc dl = go
      where
        go []       = return True
        go (m : ms) = do
            _ <- evaluate (length (_mutant m))
            modifyIORef' acc (m :)
            now <- getCurrentTime
            if now >= dl then return False else go ms

-- | Hard ceiling (seconds) on operator selection for a single file, independent
-- of the render budget.  Generous because it is a fixed cost paid once per file;
-- only pathologically large modules approach it.
genSetupCeilingSecs :: Int
genSetupCeilingSecs = 30

-- ---------------------------------------------------------------------------
-- Parallel evaluation (AC 14)
-- ---------------------------------------------------------------------------

-- | Run @--jobs N@: shard the discovered files across N isolated copies of the
-- repo and spawn one worker subprocess per shard, then merge their results.
--
-- Isolation is mandatory because the orchestrator edits files in place — workers
-- cannot share a working tree.  Each worker gets an rsync'd copy (minus @.git@,
-- @dist-newstyle@, @.mutaskell@) and runs the ordinary single-process path on
-- its shard.  The trade-off is N copies + a cold first build per worker; the win
-- appears once per-mutant build+test dominates, which it does on real repos.
runParallel :: Opts -> IO ()
runParallel opts = do
    scope <- canonicalizePath (optFile opts)
    root <- maybe (noProjectRootFound scope) return =<< findProjectRoot scope
    setCurrentDirectory root
    createDirectoryIfMissing True stateDir
    let relScope = normalise (makeRelative root scope)
    allFiles <- discoverSources opts { optFile = relScope }
    done <- readProgress
    let n       = optJobs opts
        pending = dropCompleted done allFiles
        shards  = filter (not . null) (distribute n pending)
    if null shards
        then putStrLn $ if null allFiles
            then "No Haskell source files discovered. Nothing to do."
            else "All " ++ show (length allFiles)
                ++ " discovered file(s) already done (per "
                ++ progressFile ++ "). Nothing to do."
        else do
            self <- getExecutablePath
            tmp  <- getTemporaryDirectory
            putStrLn $ "Project mode (parallel: " ++ show (length shards)
                ++ " job(s)) on " ++ scope
            putStrLn $ "  root:  " ++ root
            putStrLn $ "Discovered " ++ show (length allFiles)
                ++ " source file(s); sharding across workers.\n"
            let perJobMax = fmap (\m -> max 1 (m `div` length shards)) (optMaxMutants opts)
            jobs <- forM (zip [1 :: Int ..] shards) $ \(i, shard) -> do
                let wdir  = tmp </> ("mutaskell-job-" ++ show i)
                    listF = wdir ++ ".files"
                    resF  = wdir ++ ".result"
                removeIfExists wdir
                -- Exclude build state and, crucially, .ghc.environment.* /
                -- cabal.project.local: those bake in absolute paths to the
                -- ORIGINAL repo's dist-newstyle and would misdirect the worker's
                -- toolchain (and any hint-based tests) to the wrong build.
                callProcess "rsync"
                    [ "-a", "--delete"
                    , "--exclude", ".git", "--exclude", "dist-newstyle"
                    , "--exclude", ".mutaskell", "--exclude", ".ghc.environment.*"
                    , "--exclude", "cabal.project.local"
                    , root ++ "/", wdir ++ "/" ]
                writeFile listF (unlines shard)
                let args = [ wdir, "--jobs", "1", "--only-files", listF
                           , "--result-out", resF ] ++ passThrough opts perJobMax
                (_, _, _, ph) <- createProcess (proc self args)
                return (ph, resF, wdir, listF)
            outcomes <- forM (zip [1 :: Int ..] jobs) $ \(i, (ph, resF, wdir, listF)) -> do
                ec <- waitForProcess ph
                t  <- readResult resF
                mergeSurvivors wdir
                mergeProgress wdir
                mapM_ removeIfExists [wdir, listF, resF]
                return (i, ec, t)
            let failed   = [i | (i, ec, _) <- outcomes, ec /= ExitSuccess]
                tallies  = [t | (_, _, t) <- outcomes]
                (k, a, s, tot) = foldr add4 (0, 0, 0, 0) tallies
                msum = MAnalysisSummary
                    { _maCoveredNumMutants = -1, _maNumMutants = tot
                    , _maAlive = a, _maKilled = k, _maErrors = 0, _maSkipped = s }
            putStrLn ""
            putStrLn "==== Project mutation summary (parallel) ===="
            print msum
            putStrLn $ "Surviving mutants merged into " ++ survivorsFile
                ++ " (when any survived)."
            -- A worker that fails its baseline (or crashes) exits non-zero and
            -- writes no result; its shard would otherwise vanish silently.
            if null failed
                then do
                    doneAfter <- readProgress
                    let allDone = null (dropCompleted doneAfter allFiles)
                    when allDone clearProgress
                    applyExitPolicy opts msum
                else do
                    hPutStrLn stderr $ "ERROR: " ++ show (length failed)
                        ++ " of " ++ show (length jobs)
                        ++ " worker(s) failed (baseline failure or crash); their"
                        ++ " shards were NOT evaluated. Worker numbers: "
                        ++ show failed ++ ". The score above is incomplete."
                    exitWith (ExitFailure 3)

-- | The discovered files not yet recorded as completed, in discovery order.
dropCompleted :: [FilePath] -> [FilePath] -> [FilePath]
dropCompleted done files =
    filter (`Set.notMember` Set.fromList done) files

-- | Round-robin a list into @n@ buckets in one pass over the list.  Bucket
-- @i@ holds the elements whose 0-based index is congruent to @i@ mod @n@, in
-- their original order; with fewer files than buckets the trailing buckets
-- come back empty.
distribute :: Int -> [a] -> [[a]]
distribute n xs
    | n <= 0    = []
    | otherwise =
        [ reverse (IntMap.findWithDefault [] i buckets) | i <- [0 .. n - 1] ]
  where
    buckets = foldl' step IntMap.empty (zip [0 :: Int ..] xs)
    -- New elements prepend to their bucket; the per-bucket reverse restores
    -- file order.
    step m (i, x) = IntMap.insertWith (++) (i `mod` n) [x] m

-- | Build the worker argument list from the master's options (per-job mutant cap).
passThrough :: Opts -> Maybe Int -> [String]
passThrough opts mMax = concat
    [ optArg "--timeout"     (show <$> optTimeout opts)
    , optArg "--time-budget" (show <$> optTimeBudget opts)
    , optArg "--build-cmd"   (optBuildCmd opts)
    , optArg "--test-cmd"    (optTestCmd opts)
    , optArg "--max-mutants" (show <$> mMax)
    , if null (optTix opts) then [] else ["--tix", optTix opts]
    , ["--coverage" | optCoverage opts]
    , concatMap (\d -> ["--disable", d]) (optDisable opts)
    , concatMap (\e -> ["--enable", e]) (optEnable opts)
    ]
  where optArg flag = maybe [] (\v -> [flag, v])

-- | Read a worker's @killed alive skipped total@ result line.
readResult :: FilePath -> IO (Int, Int, Int, Int)
readResult p = do
    e <- try (readFile' p) :: IO (Either SomeException String)
    -- Parse defensively: a worker killed mid-write leaves a partial line, and a
    -- bare `read` there would throw and take the whole master run down.
    return $ case e of
        Right s | Just [k, a, sk, t] <- mapM readMaybe (words s) -> (k, a, sk, t)
        _ -> (0, 0, 0, 0)

-- | Append a worker's survivor report to the master's.
mergeSurvivors :: FilePath -> IO ()
mergeSurvivors wdir = do
    let wsv = wdir </> stateDir </> "survivors.txt"
    e <- try (readFile' wsv) :: IO (Either SomeException String)
    case e of
        Right c -> appendFile survivorsFile c
        Left _  -> return ()

-- | Append a worker's completed-file list to the master's progress, so a
-- subsequent parallel run resumes (skips files finished by a previous run).
-- The worker's copy mirrors the repo layout, so its relative paths match.
mergeProgress :: FilePath -> IO ()
mergeProgress wdir = do
    let wp = wdir </> stateDir </> "progress"
    e <- try (readFile' wp) :: IO (Either SomeException String)
    case e of
        Right c -> appendFile progressFile c
        Left _  -> return ()

add4 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
add4 (a, b, c, d) (w, x, y, z) = (a + w, b + x, c + y, d + z)

removeIfExists :: FilePath -> IO ()
removeIfExists p = do
    isDir  <- doesDirectoryExist p
    isFile <- doesFileExist p
    when isDir  (removeDirectoryRecursive p)
    when isFile (removeFile p)

-- | Restrict the discovered files to this worker's shard (@--only-files@).
-- The shard list is indexed into a set, and the discovered files keep their
-- own order — the worker evaluates in discovery order, not shard-file order.
restrictToShard :: Opts -> [FilePath] -> IO [FilePath]
restrictToShard opts files = case optOnlyFiles opts of
    Nothing -> return files
    Just p  -> do
        wanted <- Set.fromList . lines <$> readFile' p
        return (filter (`Set.member` wanted) files)

-- | Write this run's @killed alive skipped total@ for the parent (@--result-out@).
writeResult :: Opts -> MAnalysisSummary -> IO ()
writeResult opts msum = case optResultOut opts of
    Nothing -> return ()
    Just p  -> writeFile p $ unwords $ map show
        [_maKilled msum, _maAlive msum, _maSkipped msum, _maNumMutants msum]

-- ---------------------------------------------------------------------------
-- Coverage gating (AC 12)
-- ---------------------------------------------------------------------------

-- | Parsed coverage for one project run.
type CoverageSnapshot = Maybe TixIndex

-- | Parse the selected @.tix@ once for this project run.  'Nothing' means
-- coverage gating is disabled or no automatically discovered file exists.
-- A missing or unparseable @.tix@ exits with code 2 and names the file; so
-- does @--min-covered-msi@ when @--coverage@ discovery finds no file.
loadCoverage :: Opts -> IO CoverageSnapshot
loadCoverage opts = do
    mt <- resolveTix opts
    case mt of
        Nothing
            | isJust (optMinCoveredMsi opts) -> do
                hPutStrLn stderr ("Coverage: no .tix file found; " ++ missingCoverageForMinCoveredMsi)
                exitWith (ExitFailure 2)
            | otherwise -> return Nothing
        Just tix -> do
            eindex <- parseTixIndex tix
            case eindex of
                Left err    -> hPutStrLn stderr err >> exitWith (ExitFailure 2)
                Right index -> return (Just index)

-- | Uncovered spans for a module, when coverage is enabled and a @.tix@ is
-- available.  'Nothing' means "do not gate".  The parsed coverage snapshot is
-- shared by every source file in the run.
resolveUncovered :: CoverageSnapshot -> String -> IO (Maybe [Span])
resolveUncovered coverage modName = case coverage of
    Nothing       -> return Nothing
    Just index -> do
        result <- getUnCoveredPatchesFromIndex index modName
        return $ case result of
            Left _    -> Nothing
            Right spans -> spans

resolveTix :: Opts -> IO (Maybe FilePath)
resolveTix opts
    | not (null (optTix opts)) = return (Just (optTix opts))
    | optCoverage opts = do
        fs <- listDirectory "."
        return $ case filter (".tix" `isSuffixOf`) fs of
            (f : _) -> Just f
            []      -> Nothing
    | otherwise = return Nothing

-- ---------------------------------------------------------------------------
-- Command auto-detection (AC 2)
-- ---------------------------------------------------------------------------

-- | Decide the build and test commands.  Explicit flags win; otherwise detect
-- cabal vs stack from project files.
detectCommands :: Opts -> IO (String, String)
detectCommands opts = do
    isCabal <- isCabalProject
    isStack <- doesFileExist "stack.yaml"
    let (defBuild, defTest)
            | isStack && not isCabal =
                ("stack build", "stack test")
            | otherwise =
                ( "cabal build all --write-ghc-environment-files=always"
                , "cabal test all --test-show-details=direct" )
    return ( fromMaybe defBuild (optBuildCmd opts)
           , fromMaybe defTest  (optTestCmd opts) )

isCabalProject :: IO Bool
isCabalProject = do
    hasProject <- doesFileExist "cabal.project"
    if hasProject
        then return True
        else not . null <$> cabalFilesIn "."

-- | Check if a directory is a project root containing cabal.project, *.cabal, or stack.yaml.
isProjectRoot :: FilePath -> IO Bool
isProjectRoot dir = do
    hasProject <- doesFileExist (dir </> "cabal.project")
    hasStack   <- doesFileExist (dir </> "stack.yaml")
    hasCabal   <- not . null <$> cabalFilesIn dir
    return (hasProject || hasStack || hasCabal)

-- | Walk upwards from a directory to find the nearest project root.
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot dir = do
    isRoot <- isProjectRoot dir
    if isRoot
        then return (Just dir)
        else do
            let parent = takeDirectory dir
            if parent == dir
                then return Nothing
                else findProjectRoot parent

-- | Abort because no project root was found above the scope directory.
noProjectRootFound :: FilePath -> IO a
noProjectRootFound scope = do
    hPutStrLn stderr $ "Error: no project root found above " ++ scope
        ++ " (missing cabal.project, *.cabal, or stack.yaml)"
    exitWith (ExitFailure 3)

-- ---------------------------------------------------------------------------
-- Source discovery (AC 3)
-- ---------------------------------------------------------------------------

-- | Scan-work counters for one 'discoverSourcesWithStats' run — instrumentation
-- for the discovery cost (see the performance audit, finding 10).
data DiscoveryStats = DiscoveryStats
    { dsRoots :: Int   -- ^ roots actually walked, after pruning overlaps
    , dsDirs  :: Int   -- ^ distinct directories listed
    , dsFiles :: Int   -- ^ Haskell files discovered
    } deriving (Eq, Show)

-- | Resolve the scope directory relative to the current directory (project root).
resolveRelScope :: Opts -> IO FilePath
resolveRelScope opts = do
    root <- getCurrentDirectory
    let target = optFile opts
    if null target || target == "."
        then return "."
        else do
            canon <- canonicalizePath target
            let rel = normalise (makeRelative root canon)
            return (if null rel then "." else rel)

-- | True if dir could contain relScope or relScope could contain dir.
overlapsScope :: FilePath -> FilePath -> Bool
overlapsScope relScope dir
    | relScope `elem` [".", ""] = True
    | dir `elem` [".", ""]      = True
    | otherwise                 =
        dir == relScope
        || (dir ++ "/") `isPrefixOf` relScope
        || (relScope ++ "/") `isPrefixOf` dir

-- | True if the file path falls within relScope.
isInScope :: FilePath -> FilePath -> Bool
isInScope relScope p
    | relScope `elem` [".", ""] = True
    | otherwise                 =
        p == relScope || (relScope ++ "/") `isPrefixOf` p

-- | Discover Haskell source files for the project, relative to the (already
-- chdir'd) project root.  Roots are the @hs-source-dirs@ declared in every
-- @.cabal@ file, plus each package directory (covering library stanzas that omit
-- @hs-source-dirs@).  Excluded: @dist-newstyle@, @.git@, @.stack-work@, and any
-- @--exclude-dirs@.
discoverSources :: Opts -> IO [FilePath]
discoverSources opts = fst <$> discoverSourcesWithStats opts

-- | 'discoverSources' plus the scan counters above.  Overlapping roots are
-- pruned (a root that another root contains is not walked again) and each
-- directory is listed once, so a normal root-package layout walks the tree
-- once instead of once per declared source dir.  The file selection is the
-- sorted, de-duplicated union — identical to walking every root and pooling.
discoverSourcesWithStats :: Opts -> IO ([FilePath], DiscoveryStats)
discoverSourcesWithStats opts = do
    relScope <- resolveRelScope opts
    cabals <- cabalFilesIn "."
    parsed <- mapM cabalDirsOf cabals
    let libDirs  = concatMap fst parsed
        -- Test/bench source dirs to skip.  Drop "." (a test-suite with no
        -- hs-source-dirs defaults to the package dir) so we never exclude the
        -- whole tree, and drop any dir that a library/executable also builds
        -- from (e.g. mutaskell's `test-suite` lists `test app`, but `app` is the
        -- executable's own code and must still be mutated).
        testDirs = concatMap (filter (`notElem` ([".", ""] ++ libDirs)) . snd) parsed
        pkgDirs  = nub (map dirOf cabals)
        scopeRoots = [relScope | relScope `notElem` [".", ""]]
        -- With no cabal files (or none yielding a directory) fall back to walking
        -- the project root, so a plain directory of Haskell still works.
        roots0   = case nub (map canonicalDir (libDirs ++ pkgDirs ++ scopeRoots)) of
                      [] -> ["."]
                      rs -> rs
    existing <- filterM doesDirectoryExist roots0
    -- Only roots that will really be walked are counted; pruning removes
    -- containers, exclusion removes roots nothing may be collected from.
    let roots  = pruneRoots existing
        walked = [r | r <- roots, not (excluded testDirs r), overlapsScope relScope r]
    visitedRef <- newIORef Set.empty
    statsRef <- newIORef (DiscoveryStats (length walked) 0 0)
    files <- concat <$> mapM (walkDir visitedRef statsRef testDirs relScope) walked
    stats <- readIORef statsRef
    -- Set membership replaces the O(F^2) nub: the pool comes out unique and
    -- sorted, in the order files are processed.
    let found = Set.toAscList (Set.fromList files)
    return (found, stats { dsFiles = length found })
  where
    dirOf c = let d = reverse (dropWhile (/= '/') (reverse c))
              in if null d then "." else d
    -- Trailing separators removed, so "src/" and "src" compare equal.
    canonicalDir = stripTrailingSep . normalise
    stripTrailingSep = dropWhileEnd (== '/')

    -- Drop every root that another root contains.  Both sides are already
    -- canonical, so prefix equality on "dir/" decides containment; the
    -- project root "." contains every other root.
    pruneRoots rs =
        [ r | r <- rs, not (any (\o -> o /= r && containsRoot o r) rs) ]
    containsRoot o r
        | o == "."  = True
        | r == "."  = False
        | otherwise = (o ++ "/") `isPrefixOf` (r ++ "/")

    -- Directories and files that never get mutated.  testDirs are test or
    -- benchmark source dirs to prune (so test code is not mutated); they are
    -- matched as path prefixes, not bare components, to avoid excluding an
    -- unrelated src/Test.
    excluded testDirs p =
        any (`elem` pathParts p) (["dist-newstyle", ".git", ".stack-work"] ++ optExcludeDirs opts)
        || p `elem` map canonicalDir testDirs
        || any (\t -> (canonicalDir t ++ "/") `isPrefixOf` (p ++ "/")) testDirs
    pathParts = foldr splitSlash [""] . normalise
    splitSlash '/' acc = "" : acc
    splitSlash c (x:xs) = (c : x) : xs
    splitSlash c []     = [[c]]
    isHaskell p = takeExtension p `elem` [".hs", ".lhs"]
        && not ("Setup.hs" `isSuffixOf` takeFileName p)

    walkDir :: IORef (Set.Set FilePath) -> IORef DiscoveryStats
            -> [FilePath] -> FilePath -> FilePath -> IO [FilePath]
    walkDir visitedRef statsRef testDirs relScope dir = do
        isDir <- doesDirectoryExist dir
        if not isDir || excluded testDirs dir || not (overlapsScope relScope dir)
            then return []
            else do
                seen <- readIORef visitedRef
                if Set.member dir seen
                    then return []       -- another root already walked here
                    else do
                        modifyIORef' visitedRef (Set.insert dir)
                        modifyIORef' statsRef
                            (\s -> s { dsDirs = dsDirs s + 1 })
                        es <- listDirectory dir
                        fmap concat $ forM es $ \e -> do
                            let p = normalise (dir </> e)
                            d <- doesDirectoryExist p
                            if d
                                then walkDir visitedRef statsRef testDirs relScope p
                                else return [p | isHaskell p && isInScope relScope p]

-- | List @.cabal@ files directly inside a directory.
cabalFilesIn :: FilePath -> IO [FilePath]
cabalFilesIn dir = do
    exists <- doesDirectoryExist dir
    if not exists then return [] else do
        es <- listDirectory dir
        let cs = filter ((== ".cabal") . takeExtension) es
        filterM doesFileExist [normalise (dir </> c) | c <- cs]

-- | Parse @(buildable-dirs, test\/bench-dirs)@ from a cabal file (same-line
-- @hs-source-dirs@ values; comma/space separated).  Stanza-aware: dirs under
-- @library@/@executable@ are code to mutate; dirs under @test-suite@/@benchmark@
-- are returned separately so the walker can skip them — we must not mutate the
-- test code itself.  Good enough for the common layout; the package-root
-- fallback in 'discoverSources' covers files not reached by parsing.
cabalDirsOf :: FilePath -> IO ([FilePath], [FilePath])
cabalDirsOf cabal = do
    e <- try (readFile' cabal) :: IO (Either SomeException String)
    case e of
        Left _    -> return ([], [])
        Right txt -> return (go True [] [] (lines txt))
  where
    base = let d = reverse (dropWhile (/= '/') (reverse cabal))
           in if null d then "." else init d
    -- A stanza header starts at column 0 (no leading space/tab).
    isHeader l = case l of
        (c : _) -> c /= ' ' && c /= '\t'
        []      -> False
    -- test-suite / benchmark stanzas hold test code, not code-under-test.
    headerBuildable l =
        map toLower (takeWhile (/= ' ') l) `notElem` ["test-suite", "benchmark"]
    go _ libs tests [] = (reverse libs, reverse tests)
    go buildable libs tests (l : ls)
        | isHeader l = go (headerBuildable l) libs tests ls
        | "hs-source-dirs:" `isInfixOf` map toLower l =
            let ds = [ normalise (base </> d) | d <- splitFields (afterColon l), not (null d) ]
            in if buildable
                then go buildable (reverse ds ++ libs) tests ls
                else go buildable libs (reverse ds ++ tests) ls
        | otherwise = go buildable libs tests ls

afterColon :: String -> String
afterColon = drop 1 . dropWhile (/= ':')

splitFields :: String -> [String]
splitFields = words . map (\c -> if c == ',' then ' ' else c)

-- | Collapse a leading @./@ for tidy display and stable de-duplication.
normalise :: FilePath -> FilePath
normalise p = case p of
    '.' : '/' : rest -> normalise rest
    _                -> p

-- ---------------------------------------------------------------------------
-- Resume + report (AC 9)
-- ---------------------------------------------------------------------------

-- | Read the list of source files recorded as completed.
readProgress :: IO [FilePath]
readProgress = do
    exists <- doesFileExist progressFile
    if not exists then return [] else lines <$> readFile' progressFile

-- | Append a completed source file to the progress record.
recordDone :: FilePath -> IO ()
recordDone file = appendFile progressFile (file ++ "\n")

-- | Remove the progress record so subsequent runs evaluate all files.
clearProgress :: IO ()
clearProgress = removeIfExists progressFile

recordSurvivors :: String -> FilePath -> [(Mutant, Outcome)] -> IO ()
recordSurvivors origSrc file rs = do
    let alive = [m | (m, Alive) <- rs]
    unless (null alive) $
        mapM_ (appendFile survivorsFile . survivorLine origSrc file) alive

survivorLine :: String -> FilePath -> Mutant -> String
survivorLine origSrc file m =
    case firstDiff origSrc (_mutant m) of
        Just (ln, a, b) ->
            file ++ ":" ++ show ln ++ "  " ++ showMuVar (_mtype m) ++ "\n"
                ++ "    - " ++ a ++ "\n    + " ++ b ++ "\n"
        Nothing -> file ++ "  " ++ showMuVar (_mtype m) ++ " (no line diff)\n"

firstDiff :: String -> String -> Maybe (Int, String, String)
firstDiff a b =
    safeHead
        [ (i, x, y)
        | (i, x, y) <- zip3 [1 ..] (lines a ++ repeat "") (lines b ++ repeat "")
        , x /= y
        ]
  where safeHead (z : _) = Just z
        safeHead []      = Nothing

-- ---------------------------------------------------------------------------
-- Misc
-- ---------------------------------------------------------------------------

reportBudget :: Opts -> IO ()
reportBudget opts = do
    case optMaxMutants opts of
        Just n  -> putStrLn $ "  budget: at most " ++ show n ++ " mutant(s) total"
        Nothing -> return ()
    case optTimeBudget opts of
        Just s  -> putStrLn $ "  budget: stop after " ++ show s ++ "s"
        Nothing -> return ()

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

-- | Abort because the baseline build failed.  Prints the last lines of the
-- captured build log and actionable diagnostic hints.  Never returns.
baselineBuildFailed :: String -> IO a
baselineBuildFailed cmd = do
    hPutStrLn stderr $ unlines
        [ ""
        , "Baseline build FAILED."
        , ""
        , "  Command: " ++ cmd
        ]
    printLogTail
    hints <- buildFailureHints cmd
    hPutStrLn stderr $ unlines $
        [ "  Hints:" ] ++ hints ++
        [ ""
        , "  Full output: " ++ execLog
        ]
    exitWith (ExitFailure 3)

-- | Choose build-failure hints based on what the captured log contains.
-- Promotes the single most likely fix to the top rather than listing everything.
buildFailureHints :: String -> IO [String]
buildFailureHints cmd = do
    logContent <- readLogContent
    return $ case diagnoseBuildLog logContent of
        SolverFailure ->
            [ "    - The dependency solver could not find a valid build plan."
            , "      Your Hackage index may be stale.  Try:"
            , "        cabal update"
            , "      then re-run mutaskell."
            ]
        GhcVersionGap ->
            [ "    - A dependency is not available for the installed GHC."
            , "      The project may require a different GHC version.  Try:"
            , "        ghcup run --ghc <version> -- " ++ cmd
            , "      or override: --build-cmd \"ghcup run --ghc <version> -- " ++ cmd ++ "\""
            ]
        UnknownBuildFailure ->
            [ "    - Run the command above in this directory to see the full"
            , "      error without mutaskell in the way."
            , "    - If the build command is wrong (auto-detected from"
            , "      cabal/stack files), override with: --build-cmd \"<cmd>\""
            ]

-- | Possible diagnoses for a failed baseline build.
data BuildDiagnosis = SolverFailure | GhcVersionGap | UnknownBuildFailure

-- | Inspect the captured build output for known failure signatures.
diagnoseBuildLog :: String -> BuildDiagnosis
diagnoseBuildLog logContent
    | "Could not resolve dependencies" `isInfixOf` logContent = SolverFailure
    | "No compiler found"              `isInfixOf` logContent = GhcVersionGap
    | "ghc: could not execute"         `isInfixOf` logContent = GhcVersionGap
    | otherwise                                                = UnknownBuildFailure

-- | Read the full content of 'execLog', or empty string if absent.
readLogContent :: IO String
readLogContent = do
    exists <- doesFileExist execLog
    if exists then readFile' execLog else return ""

-- | Abort because the baseline test run failed or timed out.  Never returns.
baselineTestFailed :: String -> Bool -> IO a
baselineTestFailed cmd timedOut = do
    hPutStrLn stderr $ unlines
        [ ""
        , if timedOut
            then "Baseline test suite TIMED OUT."
            else "Baseline test suite FAILED."
        , ""
        , "  Command: " ++ cmd
        ]
    printLogTail
    hPutStrLn stderr $ unlines $
        [ "  Hints:" ] ++
        ( if timedOut
            then [ "    - The test suite exceeded the configured --timeout."
                 , "      Remove --timeout to wait indefinitely, or narrow the scope:"
                 , "        --test-cmd \"cabal test <pkg> --test-show-details=direct\""
                 ]
            else [ "    - The suite must be green before mutation testing can start."
                 , "    - Fix failing tests first, or narrow the scope:"
                 , "        --test-cmd \"cabal test <pkg> --test-show-details=direct\""
                 ] ) ++
        [ ""
        , "  Full output: " ++ execLog
        ]
    exitWith (ExitFailure 3)

-- | Print the last 15 lines of 'execLog' to stderr, prefixed for readability.
printLogTail :: IO ()
printLogTail = do
    content <- readLogContent
    let tailLines = drop (max 0 (length ls - 15)) ls where ls = lines content
    unless (null tailLines) $ do
        hPutStrLn stderr "  Last output:"
        mapM_ (hPutStrLn stderr . ("    " ++)) tailLines
        hPutStrLn stderr ""

