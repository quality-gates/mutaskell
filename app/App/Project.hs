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
import Data.List (isInfixOf, isSuffixOf, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Timeout (timeout)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , setCurrentDirectory
    )
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, readFile', stderr)

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

-- | Run mutation testing across a whole project directory.
runProject :: Opts -> IO ()
runProject opts = do
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

    files <- discoverSources opts
    when (null files) $ do
        hPutStrLn stderr "No Haskell source files discovered. Nothing to do."
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
            sampled <- genWithinBudget genBudgetSecs $ do
                ms <- genSampledMutantsGated cfg muncov ast
                return (applyDisableEnable (optDisable opts) (optEnable opts) ms)
            if null sampled
                then do
                    recordDone file
                    return []
                else do
                    hPutStrLn stderr $ "FILE " ++ file ++ ": "
                        ++ show (length sampled) ++ " mutant(s)"
                    rs <- evaluateFile file buildCmd testCmd mtimeout deadline
                            file origSrc sampled
                        `finally` restore file origSrc
                    modifyIORef' budgetRef (subtract (length rs))
                    recordSurvivors origSrc file rs
                    -- Only record as done if we evaluated the whole file
                    -- (a budget cut mid-file leaves it for the resume).
                    when (length rs == length sampled) (recordDone file)
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
            sampled <- genWithinBudget genBudgetSecs $ do
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
-- until @secs@ elapses, returning however many were produced in time.  A hard
-- ceiling at 3x the budget guards against the up-front operator-building cost
-- running away on a pathological file.
genWithinBudget :: Int -> IO [Mutant] -> IO [Mutant]
genWithinBudget secs act = do
    deadline <- addUTCTime (fromIntegral secs) <$> getCurrentTime
    r <- timeout (3 * secs * 1000000) (act >>= forceUntil deadline)
    return (fromMaybe [] r)
  where
    forceUntil _ [] = return []
    forceUntil dl (m : rest) = do
        _ <- evaluate (length (_mutant m))
        now <- getCurrentTime
        if now >= dl
            then return [m]
            else (m :) <$> forceUntil dl rest

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
    parsedDirs <- concat <$> mapM hsSourceDirsOf cabals
    let pkgDirs = nub (map dirOf cabals)
        -- With no cabal files (or none yielding a directory) fall back to walking
        -- the project root, so a plain directory of Haskell still works.
        roots0  = case nub (parsedDirs ++ pkgDirs) of
                      [] -> ["."]
                      rs -> rs
    roots <- filterM doesDirectoryExist roots0
    files <- concat <$> mapM (findHaskell opts) roots
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

-- | Parse @hs-source-dirs@ values from a cabal file (same-line values only;
-- comma/space separated).  Good enough for the common layout; the package-root
-- fallback in 'discoverSources' covers the rest.
hsSourceDirsOf :: FilePath -> IO [FilePath]
hsSourceDirsOf cabal = do
    e <- try (readFile' cabal) :: IO (Either SomeException String)
    case e of
        Left _    -> return []
        Right txt -> return
            [ normalise (base </> d)
            | l <- lines txt
            , let low = map toLowerC l
            , "hs-source-dirs:" `isInfixOf` low
            , d <- splitFields (afterColon l)
            , not (null d)
            ]
  where
    base = let d = reverse (dropWhile (/= '/') (reverse cabal))
           in if null d then "." else init d
    toLowerC c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

afterColon :: String -> String
afterColon = drop 1 . dropWhile (/= ':')

splitFields :: String -> [String]
splitFields = words . map (\c -> if c == ',' then ' ' else c)

-- | Recursively find @.hs@/@.lhs@ files under a directory, honouring exclusions.
findHaskell :: Opts -> FilePath -> IO [FilePath]
findHaskell opts dir = do
    isDir <- doesDirectoryExist dir
    if not isDir || excluded dir
        then return []
        else do
            es <- listDirectory dir
            fmap concat $ forM es $ \e -> do
                let p = normalise (dir </> e)
                d <- doesDirectoryExist p
                if d
                    then findHaskell opts p
                    else return [p | isHaskell p]
  where
    excluded p =
        any (`elem` pathParts p) (["dist-newstyle", ".git", ".stack-work"] ++ optExcludeDirs opts)
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
