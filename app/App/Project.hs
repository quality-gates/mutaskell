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
    ( runProject
    , runProjectDryRun
    ) where

import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (filterM, foldM, forM, unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    , removeFile
    , setCurrentDirectory
    )
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, readFile', stderr)
import System.Process (callProcess, createProcess, proc, waitForProcess)

import App.Exit (applyExitPolicy)
import App.Filter (applyDisableEnable)
import App.Opts (Opts (..))
import App.Orchestrator
    ( Outcome (..)
    , evaluateFile
    , runCmd
    , restore
    , stateDir
    , summarise
    )
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary (..))
import Test.Mutaskell.Config (Config (..), defaultConfig, showMuVar)
import Test.Mutaskell.Mutation (genSampledMutantsGated, getASTFromFile, getModuleName)
import Test.Mutaskell.Tix (Span, getUnCoveredPatches)
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
    | optJobs opts > 1 && optOnlyFiles opts == Nothing = runParallel opts
    | otherwise                                        = runSerial opts

-- | Single-process project run (also used as each parallel worker, restricted to
-- its shard via @--only-files@).
runSerial :: Opts -> IO ()
runSerial opts = do
    root <- canonicalizePath (optFile opts)
    setCurrentDirectory root
    createDirectoryIfMissing True stateDir
    (buildCmd, testCmd) <- detectCommands opts
    let mtimeout = fmap (* 1000000) (optTimeout opts)

    putStrLn $ "Project mode on " ++ root
    putStrLn $ "  build: " ++ buildCmd
    putStrLn $ "  test:  " ++ testCmd
    reportBudget opts
    putStrLn ""

    allFiles <- discoverSources opts
    files <- restrictToShard opts allFiles
    when (null files) $ do
        hPutStrLn stderr "No Haskell source files discovered. Nothing to do."
        maybe (return ()) (`writeFile` "0 0 0 0") (optResultOut opts)
        exitWith ExitSuccess

    done <- readProgress
    let pending = filter (`notElem` done) files
    putStrLn $ "Discovered " ++ show (length files) ++ " source file(s); "
        ++ show (length done) ++ " already done, "
        ++ show (length pending) ++ " pending.\n"

    -- Baseline runs exactly once for the whole project (AC 10), not per file.
    putStrLn "Baseline: building unmodified project..."
    b0 <- runCmd Nothing buildCmd
    when (b0 /= ExitSuccess) $ abort 3
        "Baseline build failed on the unmodified project. Fix the build first."
    putStrLn "Baseline: running the test suite on unmodified project..."
    t0 <- runCmd mtimeout testCmd
    when (t0 /= ExitSuccess) $ abort 3
        "Baseline tests failed (or timed out) on the unmodified project. The suite must be green to start."
    putStrLn "Baseline OK.\n"

    -- Budget state shared across files.
    start <- getCurrentTime
    let deadline = fmap (\s -> addUTCTime (fromIntegral s) start) (optTimeBudget opts)
        totalBudget = fromMaybe maxBound (optMaxMutants opts)
    budgetRef <- newIORef totalBudget

    -- Each file's evaluation restores the original after every mutant and in a
    -- `finally`, so an interrupt cannot leave mutated source behind (AC 6).
    -- Between files no file is in a mutated state, so no extra guard is needed.
    results <- walk opts buildCmd testCmd mtimeout deadline budgetRef pending

    let msum = summarise results
    putStrLn ""
    putStrLn "==== Project mutation summary ===="
    print msum
    putStrLn $ "Surviving mutants written to " ++ survivorsFile
        ++ " (when any survived)."
    writeResult opts msum
    applyExitPolicy opts msum

-- | Walk pending files, accumulating outcomes and honouring the budget.
walk
    :: Opts -> String -> String -> Maybe Int -> Maybe UTCTime
    -> IORef Int -> [FilePath] -> IO [(Mutant, Outcome)]
walk opts buildCmd testCmd mtimeout deadline budgetRef = go []
  where
    go acc [] = return (reverse acc)
    go acc (f : fs) = do
        stop <- shouldStop deadline budgetRef
        if stop
            then do
                hPutStrLn stderr "Budget exhausted; stopping with a partial result."
                return (reverse acc)
            else do
                rs <- processFile opts buildCmd testCmd mtimeout deadline budgetRef f
                go (reverse rs ++ acc) fs

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
-- Any failure is logged and the file skipped, so the run survives bad files
-- (AC 4).
processFile
    :: Opts -> String -> String -> Maybe Int -> Maybe UTCTime
    -> IORef Int -> FilePath -> IO [(Mutant, Outcome)]
processFile opts buildCmd testCmd mtimeout deadline budgetRef file = do
    e <- try (processFile' opts buildCmd testCmd mtimeout deadline budgetRef file)
    case e of
        Right rs -> return rs
        Left (ex :: SomeException) -> do
            hPutStrLn stderr $ "SKIP " ++ file ++ ": " ++ show ex
            return []

processFile'
    :: Opts -> String -> String -> Maybe Int -> Maybe UTCTime
    -> IORef Int -> FilePath -> IO [(Mutant, Outcome)]
processFile' opts buildCmd testCmd mtimeout deadline budgetRef file = do
    origSrc <- readFile' file
    eAst <- getASTFromFile file
    case eAst of
        Left err -> do
            hPutStrLn stderr $ "SKIP " ++ file ++ " (parse): " ++ firstLine err
            return []
        Right ast -> do
            remaining <- readIORef budgetRef
            let perFileCap = min remaining (maxNumMutants defaultConfig)
                cfg        = defaultConfig { maxNumMutants = perFileCap }
            muncov <- resolveUncovered opts (getModuleName ast)
            (genComplete, sampled) <- genWithinBudget genBudgetSecs $ do
                ms <- genSampledMutantsGated cfg muncov ast
                return (applyDisableEnable (optDisable opts) (optEnable opts) ms)
            if null sampled
                then do
                    -- Record done only if generation genuinely finished (zero
                    -- mutants), not if it was time-truncated — otherwise a slow
                    -- file is silently skipped forever on resume.
                    when genComplete (recordDone file)
                    return []
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
                    return rs

-- | Dry run over a project: discover files and report per-file generation counts
-- without building or testing.  Cheap way to verify discovery + bounded
-- generation (AC 3, AC 13) on a repo that is not built.
runProjectDryRun :: Opts -> IO ()
runProjectDryRun opts = do
    root <- canonicalizePath (optFile opts)
    setCurrentDirectory root
    files <- discoverSources opts
    putStrLn $ "Project dry-run on " ++ root
    putStrLn $ "Discovered " ++ show (length files) ++ " source file(s).\n"
    total <- foldM (countFile opts) 0 files
    putStrLn $ "\nTotal generated mutants (sampled per file): " ++ show total

countFile :: Opts -> Int -> FilePath -> IO Int
countFile opts acc file = do
    e <- try (dryCount opts file) :: IO (Either SomeException (Maybe Int))
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

dryCount :: Opts -> FilePath -> IO (Maybe Int)
dryCount opts file = do
    eAst <- getASTFromFile file
    case eAst of
        Left _    -> return Nothing
        Right ast -> do
            let cap = fromMaybe (maxNumMutants defaultConfig) (optMaxMutants opts)
                cfg = defaultConfig { maxNumMutants = cap }
            muncov <- resolveUncovered opts (getModuleName ast)
            (_, sampled) <- genWithinBudget genBudgetSecs $ do
                ms <- genSampledMutantsGated cfg muncov ast
                return (applyDisableEnable (optDisable opts) (optEnable opts) ms)
            return (Just (length sampled))

-- | Soft per-file generation budget (seconds).  Generation is bounded so no
-- single file dominates the run: the operator-level sampling caps the candidate
-- count, but applying and rendering each mutant is a full-AST traversal, so a
-- huge module can still be slow.  We render mutants until this budget elapses
-- and proceed with however many we have (AC 13) rather than skipping the file.
genBudgetSecs :: Int
genBudgetSecs = 5

-- | Run a mutant-generating action and force rendered mutants one at a time
-- until @secs@ elapses, returning @(completed, mutants)@.  @completed@ is 'False'
-- if the soft deadline cut generation short or the hard ceiling (3x the budget,
-- guarding a runaway up-front operator build) fired — so the caller can avoid
-- recording a time-truncated file as fully done (which would skip it forever on
-- resume even though nothing, or only part, was generated).
genWithinBudget :: Int -> IO [Mutant] -> IO (Bool, [Mutant])
genWithinBudget secs act = do
    deadline <- addUTCTime (fromIntegral secs) <$> getCurrentTime
    r <- timeout (3 * secs * 1000000) (act >>= forceUntil deadline)
    return (fromMaybe (False, []) r)
  where
    forceUntil _ [] = return (True, [])
    forceUntil dl (m : rest) = do
        _ <- evaluate (length (_mutant m))
        now <- getCurrentTime
        if now >= dl
            then return (False, [m])
            else do
                (full, ms) <- forceUntil dl rest
                return (full, m : ms)

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
    root <- canonicalizePath (optFile opts)
    setCurrentDirectory root
    createDirectoryIfMissing True stateDir
    allFiles <- discoverSources opts
    done <- readProgress
    let n       = optJobs opts
        pending = filter (`notElem` done) allFiles
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
                ++ " job(s)) on " ++ root
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
                then applyExitPolicy opts msum
                else do
                    hPutStrLn stderr $ "ERROR: " ++ show (length failed)
                        ++ " of " ++ show (length jobs)
                        ++ " worker(s) failed (baseline failure or crash); their"
                        ++ " shards were NOT evaluated. Worker numbers: "
                        ++ show failed ++ ". The score above is incomplete."
                    exitWith (ExitFailure 3)

-- | Round-robin a list into @n@ buckets.
distribute :: Int -> [a] -> [[a]]
distribute n xs =
    [ [x | (j, x) <- zip [0 :: Int ..] xs, j `mod` n == i] | i <- [0 .. n - 1] ]

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
restrictToShard :: Opts -> [FilePath] -> IO [FilePath]
restrictToShard opts files = case optOnlyFiles opts of
    Nothing -> return files
    Just p  -> do
        wanted <- lines <$> readFile' p
        return (filter (`elem` wanted) files)

-- | Write this run's @killed alive skipped total@ for the parent (@--result-out@).
writeResult :: Opts -> MAnalysisSummary -> IO ()
writeResult opts msum = case optResultOut opts of
    Nothing -> return ()
    Just p  -> writeFile p $ unwords $ map show
        [_maKilled msum, _maAlive msum, _maSkipped msum, _maNumMutants msum]

-- ---------------------------------------------------------------------------
-- Coverage gating (AC 12)
-- ---------------------------------------------------------------------------

-- | Uncovered spans for a module, when coverage is enabled and a @.tix@ is
-- available.  'Nothing' means "do not gate".  Driven by @--tix FILE@ or
-- @--coverage@ (auto-discover a @.tix@ in the project root).
resolveUncovered :: Opts -> String -> IO (Maybe [Span])
resolveUncovered opts modName = do
    mt <- resolveTix opts
    case mt of
        Nothing  -> return Nothing
        Just tix -> either (const Nothing) id <$> getUnCoveredPatches tix modName

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

-- ---------------------------------------------------------------------------
-- Source discovery (AC 3)
-- ---------------------------------------------------------------------------

-- | Discover Haskell source files for the project, relative to the (already
-- chdir'd) project root.  Roots are the @hs-source-dirs@ declared in every
-- @.cabal@ file, plus each package directory (covering library stanzas that omit
-- @hs-source-dirs@).  Excluded: @dist-newstyle@, @.git@, @.stack-work@, and any
-- @--exclude-dirs@.
discoverSources :: Opts -> IO [FilePath]
discoverSources opts = do
    cabals <- cabalFilesIn "."
    parsed <- mapM cabalDirsOf cabals
    let libDirs  = concatMap fst parsed
        -- Test/bench source dirs to skip.  Drop "." (a test-suite with no
        -- hs-source-dirs defaults to the package dir) so we never exclude the
        -- whole tree.
        testDirs = filter (`notElem` [".", ""]) (concatMap snd parsed)
        pkgDirs  = nub (map dirOf cabals)
        -- With no cabal files (or none yielding a directory) fall back to walking
        -- the project root, so a plain directory of Haskell still works.
        roots0   = case nub (libDirs ++ pkgDirs) of
                      [] -> ["."]
                      rs -> rs
    roots <- filterM doesDirectoryExist roots0
    files <- concat <$> mapM (findHaskell opts testDirs) roots
    return (sort (nub files))
  where
    dirOf c = let d = reverse (dropWhile (/= '/') (reverse c))
              in if null d then "." else d

-- | List @.cabal@ files directly inside a directory.
cabalFilesIn :: FilePath -> IO [FilePath]
cabalFilesIn dir = do
    exists <- doesDirectoryExist dir
    if not exists then return [] else do
        es <- listDirectory dir
        let cs = filter ((== ".cabal") . takeExtension) es
        return [normalise (dir </> c) | c <- cs]

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
    toLowerC c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    -- A stanza header starts at column 0 (no leading space/tab).
    isHeader l = case l of
        (c : _) -> c /= ' ' && c /= '\t'
        []      -> False
    -- test-suite / benchmark stanzas hold test code, not code-under-test.
    headerBuildable l =
        map toLowerC (takeWhile (/= ' ') l) `notElem` ["test-suite", "benchmark"]
    go _ libs tests [] = (reverse libs, reverse tests)
    go buildable libs tests (l : ls)
        | isHeader l = go (headerBuildable l) libs tests ls
        | "hs-source-dirs:" `isInfixOf` map toLowerC l =
            let ds = [ normalise (base </> d) | d <- splitFields (afterColon l), not (null d) ]
            in if buildable
                then go buildable (reverse ds ++ libs) tests ls
                else go buildable libs (reverse ds ++ tests) ls
        | otherwise = go buildable libs tests ls

afterColon :: String -> String
afterColon = drop 1 . dropWhile (/= ':')

splitFields :: String -> [String]
splitFields = words . map (\c -> if c == ',' then ' ' else c)

-- | Recursively find @.hs@/@.lhs@ files under a directory, honouring exclusions.
-- @testDirs@ are test\/benchmark source dirs to prune (so test code is not
-- mutated); they are matched as path prefixes, not bare components, to avoid
-- excluding an unrelated @src\/Test@.
findHaskell :: Opts -> [FilePath] -> FilePath -> IO [FilePath]
findHaskell opts testDirs dir = do
    isDir <- doesDirectoryExist dir
    if not isDir || excluded dir
        then return []
        else do
            es <- listDirectory dir
            fmap concat $ forM es $ \e -> do
                let p = normalise (dir </> e)
                d <- doesDirectoryExist p
                if d
                    then findHaskell opts testDirs p
                    else return [p | isHaskell p]
  where
    excluded p =
        any (`elem` pathParts p) (["dist-newstyle", ".git", ".stack-work"] ++ optExcludeDirs opts)
        || normalise p `elem` map normalise testDirs
        || any (\t -> (normalise t ++ "/") `isPrefixOf` (normalise p ++ "/")) testDirs
    pathParts = foldr splitSlash [""] . normalise
    splitSlash '/' acc = "" : acc
    splitSlash c (x:xs) = (c : x) : xs
    splitSlash c []     = [[c]]
    isHaskell p = takeExtension p `elem` [".hs", ".lhs"]
        && not ("Setup.hs" `isSuffixOf` takeFileName p)

-- | Collapse a leading @./@ for tidy display and stable de-duplication.
normalise :: FilePath -> FilePath
normalise p = case p of
    '.' : '/' : rest -> normalise rest
    _                -> p

-- ---------------------------------------------------------------------------
-- Resume + report (AC 9)
-- ---------------------------------------------------------------------------

readProgress :: IO [FilePath]
readProgress = do
    exists <- doesFileExist progressFile
    if not exists then return [] else lines <$> readFile' progressFile

recordDone :: FilePath -> IO ()
recordDone file = appendFile progressFile (file ++ "\n")

recordSurvivors :: String -> FilePath -> [(Mutant, Outcome)] -> IO ()
recordSurvivors origSrc file rs = do
    let alive = [m | (m, Alive) <- rs]
    unless (null alive) $
        mapM_ (\m -> appendFile survivorsFile (survivorLine origSrc file m)) alive

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

abort :: Int -> String -> IO a
abort code msg = do
    hPutStrLn stderr msg
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)
