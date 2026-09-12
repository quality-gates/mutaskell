-- | End-to-end tests for project mode, run against a synthetic project with
-- cheap build/test commands so no real toolchain is involved:
--
--   * build command @true@ — every mutant compiles
--   * test command greps a sentinel line in @M1.hs@ — green on the original
--     source (the baseline must pass), red once @M1.hs@ is mutated.  Mutants of
--     the other files leave the sentinel alone, so some survive.
--
-- The seam is 'runProject' observed through its run-state files
-- (@--result-out@ counts, the survivor report, and the progress log), so these
-- tests pin the aggregate behaviour that must not move when summary state is
-- restructured.
module Test.Mutaskell.ProjectSpec where

import Control.Exception (finally, try)
import Control.Monad (forM_)
import Data.List (isInfixOf, sort)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getCurrentDirectory
    , listDirectory
    , setCurrentDirectory
    , withCurrentDirectory
    )
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (emptySystemTempFile, withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

import qualified App.Orchestrator as Orchestrator
import App.Orchestrator (stateDir)
import App.Opts (Opts (..), defaultOpts)
import qualified App.Project as Project
import App.Project
    ( DiscoveryStats (..)
    , discoverSourcesWithStats
    , distribute
    , filterDiff
    , findProjectRoot
    , restrictToShard
    , runProject
    , runProjectDryRun
    )
import Test.Mutaskell.Tix (tixReadCount)
import Test.Mutaskell.Utils.Print (catchOutputStr)

-- | A small module with constructs every mutator family can hit.
sampleModule :: String -> String
sampleModule name = unlines
    [ "module " ++ name ++ " where"
    , ""
    , "f :: Int -> Int"
    , "f x = x + 1"
    , ""
    , "g :: Int -> Int"
    , "g x = x * 2"
    , ""
    , "h :: Int -> Bool"
    , "h x = x > 0"
    ]

-- | Write @n@ independent source files into @root@.
makeProject :: FilePath -> Int -> IO ()
makeProject root n = do
    writeFile (root </> "cabal.project") "packages: .\n"
    forM_ [1 .. n] $ \i ->
        writeFile (root </> ("M" ++ show i ++ ".hs")) (sampleModule ("M" ++ show i))

-- | Project-mode options driving the synthetic project.  The test command is
-- green exactly while @M1.hs@ still holds its original @x + 1@ line.
projectOpts :: FilePath -> FilePath -> Opts
projectOpts root resultOut = defaultOpts
    { optFile      = root
    , optBuildCmd  = Just "true"
    , optTestCmd   = Just "grep -q 'x + 1' M1.hs"
    , optResultOut = Just resultOut
    }

-- | A cabal file whose package dir (@. @) overlaps the declared library and
-- executable source dirs — the layout mutaskell's own repo has.
writeCabalProject :: FilePath -> IO ()
writeCabalProject root =
    writeFile (root </> "overlap.cabal") $ unlines
        [ "cabal-version: 2.4"
        , "name: overlap"
        , "version: 0.1"
        , ""
        , "library"
        , "    hs-source-dirs: src"
        , "    exposed-modules: A"
        , ""
        , "executable oexe"
        , "    hs-source-dirs: app"
        , "    main-is: Main.hs"
        , ""
        , "test-suite otest"
        , "    type: exitcode-stdio-1.0"
        , "    hs-source-dirs: test"
        , "    main-is: Spec.hs"
        ]

-- | Write @path -> body@ files under @root@, creating parent directories.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles root files = forM_ files $ \(p, body) -> do
    createDirectoryIfMissing True (root </> takeDirectory p)
    writeFile (root </> p) body

-- | Run project mode and restore the process working directory afterwards
-- (project mode chdirs into the target project).
runProjectRestoring :: Opts -> IO ()
runProjectRestoring opts = do
    old <- getCurrentDirectory
    runProject opts `finally` setCurrentDirectory old

-- | Run project dry-run and restore the process working directory afterwards.
runProjectDryRunRestoring :: Opts -> IO ()
runProjectDryRunRestoring opts = do
    old <- getCurrentDirectory
    runProjectDryRun opts `finally` setCurrentDirectory old

-- | Read the @killed alive skipped total@ line written for the parent.
readCounts :: FilePath -> IO (Int, Int, Int, Int)
readCounts p = do
    s <- readFile p
    return $ case map read (words s) of
        [k, a, sk, t] -> (k, a, sk, t)
        _             -> error ("bad result line: " ++ s)

spec :: Spec
spec = describe "runProject (serial)" $ do
    it "finds first changed lines and terminates when sources have no line diff" $ do
        let check firstDiff a b expected =
                firstDiff a b `shouldBe` expected
        forM_ [Orchestrator.firstDiff, Project.firstDiff] $ \firstDiff -> do
            check firstDiff "" "" Nothing
            check firstDiff "same\nlines" "same\nlines" Nothing
            check firstDiff "old\nvalue" "new\nvalue"
                (Just (1, "old", "new"))
            check firstDiff "first\nold\nlast" "first\nnew\nlast"
                (Just (2, "old", "new"))
            check firstDiff "only line" "only line\nadded"
                (Just (2, "", "added"))
            check firstDiff "first line\nremoved" "first line"
                (Just (2, "removed", ""))

    it "evaluates every pending file and reports kill and escape counts" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            let resF = root </> "result.txt"
            runProjectRestoring (projectOpts root resF)
            (killed, alive, skipped, total) <- readCounts resF
            total `shouldSatisfy` (> 0)
            killed `shouldSatisfy` (> 0)   -- M1 mutants that touch the sentinel
            alive `shouldSatisfy` (> 0)    -- M2/M3 mutants, and untouched sentinels
            skipped `shouldBe` 0           -- the build command cannot fail
            killed + alive + skipped `shouldBe` total

    it "reads project coverage once across multiple source files" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            writeFile (root </> "coverage.tix") "Tix []"
            before <- tixReadCount
            runProjectRestoring $ (projectOpts root (root </> "result.txt"))
                { optTix = "coverage.tix" }
            after <- tixReadCount
            after - before `shouldBe` 1

    it "clears progress on a completed run" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            let resF = root </> "result.txt"
            runProjectRestoring (projectOpts root resF)
            exists <- doesFileExist (root </> stateDir </> "progress")
            exists `shouldBe` False

    it "produces identical mutant counts and gate evaluations on a second run in an unchanged tree" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            let resF1 = root </> "result1.txt"
                resF2 = root </> "result2.txt"
                opts1 = (projectOpts root resF1) { optMinMsi = Just 10 }
                opts2 = (projectOpts root resF2) { optMinMsi = Just 10 }
            res1 <- try (runProjectRestoring opts1) :: IO (Either ExitCode ())
            res1 `shouldBe` Right ()
            counts1 <- readCounts resF1
            res2 <- try (runProjectRestoring opts2) :: IO (Either ExitCode ())
            res2 `shouldBe` Right ()
            counts2 <- readCounts resF2
            counts2 `shouldBe` counts1

    it "preserves progress on an interrupted run and skips completed files on resume" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            let resF1 = root </> "result1.txt"
                resF2 = root </> "result2.txt"
                opts1 = (projectOpts root resF1) { optMaxMutants = Just 1 }
                opts2 = projectOpts root resF2
            runProjectRestoring opts1
            (_, _, _, total1) <- readCounts resF1
            total1 `shouldBe` 1
            done1 <- lines <$> strictRead (root </> stateDir </> "progress")
            length done1 `shouldBe` 1

            -- Second run resumes and evaluates remaining files
            runProjectRestoring opts2
            (_, _, _, total2) <- readCounts resF2
            total2 `shouldSatisfy` (> 0)
            -- Once all files are complete, progress is cleared
            exists <- doesFileExist (root </> stateDir </> "progress")
            exists `shouldBe` False

    it "prints nothing to do without emitting a summary when all discovered files are already done" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            createDirectoryIfMissing True (root </> stateDir)
            writeFile (root </> stateDir </> "progress") "M1.hs\nM2.hs\nM3.hs\n"
            let resF = root </> "result.txt"
                opts = (projectOpts root resF) { optMinMsi = Just 50 }
            (_, out) <- catchOutputStr (runProjectRestoring opts)
            out `shouldSatisfy` ("All 3 discovered file(s) already done (per .mutaskell/progress). Nothing to do." `isInfixOf`)
            out `shouldNotSatisfy` ("==== Project mutation summary ====" `isInfixOf`)

    it "reports surviving mutants and fails under --fail-on-escaped" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 2
            let resF = root </> "result.txt"
                opts = (projectOpts root resF) { optFailOnEscape = True }
            ec <- try (runProjectRestoring opts)
            ec `shouldBe` Left (ExitFailure 4)
            (killed, alive, skipped, total) <- readCounts resF
            alive `shouldSatisfy` (> 0)
            killed + alive + skipped `shouldBe` total
            survivors <- strictRead (root </> stateDir </> "survivors.txt")
            survivors `shouldSatisfy`
                containsAll ["    - ", "    + "]

    it "mutates only files under a scope subdirectory while running commands from project root" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            writeFile (root </> "cabal.project") "packages: .\n"
            let srcDir = root </> "src"
                otherDir = root </> "other"
            createDirectoryIfMissing True srcDir
            createDirectoryIfMissing True otherDir
            writeFile (srcDir </> "M1.hs") (sampleModule "M1")
            writeFile (otherDir </> "M2.hs") (sampleModule "M2")
            let resF = root </> "result.txt"
                opts = (projectOpts root resF)
                    { optFile = srcDir
                    , optTestCmd = Just "grep -q 'x + 1' src/M1.hs"
                    }
            (_, out) <- catchOutputStr (runProjectRestoring opts)
            out `shouldSatisfy` (("Project mode on " ++ srcDir) `isInfixOf`)
            out `shouldSatisfy` (("root:  " ++ root) `isInfixOf`)
            (killed, alive, skipped, total) <- readCounts resF
            total `shouldSatisfy` (> 0)
            -- Only M1.hs mutants evaluated (11 mutants), not M2.hs
            total `shouldBe` 11

    it "mutates only files changed relative to --git-diff-base" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 2
            withCurrentDirectory root $ do
                callProcess "git" ["init", "-q"]
                callProcess "git" ["config", "user.email", "test@example.com"]
                callProcess "git" ["config", "user.name", "test"]
                callProcess "git" ["add", "."]
                callProcess "git" ["commit", "-qm", "base"]
                writeFile "M1.hs" (sampleModule "M1" ++ "\n-- modified\n")
            let resF = root </> "result.txt"
                opts = (projectOpts root resF)
                    { optGitDiffBase = Just "HEAD"
                    }
            runProjectRestoring opts
            (_, _, _, total) <- readCounts resF
            total `shouldBe` 11

    it "only counts mutants for files modified relative to --git-diff-base in dry-run" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 2
            withCurrentDirectory root $ do
                callProcess "git" ["init", "-q"]
                callProcess "git" ["config", "user.email", "test@example.com"]
                callProcess "git" ["config", "user.name", "test"]
                callProcess "git" ["add", "."]
                callProcess "git" ["commit", "-qm", "base"]
                writeFile "M1.hs" (sampleModule "M1" ++ "\n-- modified\n")
            let opts = defaultOpts
                    { optFile = root
                    , optDryRun = True
                    , optGitDiffBase = Just "HEAD"
                    }
            (_, out) <- catchOutputStr (runProjectDryRunRestoring opts)
            out `shouldSatisfy` ("Discovered 1 source file(s)." `isInfixOf`)
            out `shouldSatisfy` ("Total generated mutants (sampled per file): 11" `isInfixOf`)

    it "discovers 0 files and exits cleanly on clean working tree relative to --git-diff-base" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 2
            withCurrentDirectory root $ do
                callProcess "git" ["init", "-q"]
                callProcess "git" ["config", "user.email", "test@example.com"]
                callProcess "git" ["config", "user.name", "test"]
                callProcess "git" ["add", "."]
                callProcess "git" ["commit", "-qm", "base"]
            let resF = root </> "result.txt"
                opts = (projectOpts root resF)
                    { optGitDiffBase = Just "HEAD"
                    }
            (ec, _) <- catchOutputStr (try (runProjectRestoring opts) :: IO (Either ExitCode ()))
            case ec of
                Left ExitSuccess -> return ()
                Right ()         -> return ()
                Left other       -> expectationFailure ("expected ExitSuccess, got " ++ show other)
            (killed, alive, skipped, total) <- readCounts resF
            (killed, alive, skipped, total) `shouldBe` (0, 0, 0, 0)

    it "restricts mutants within changed files to modified lines when --git-diff-lines is set" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 2
            withCurrentDirectory root $ do
                callProcess "git" ["init", "-q"]
                callProcess "git" ["config", "user.email", "test@example.com"]
                callProcess "git" ["config", "user.name", "test"]
                callProcess "git" ["add", "."]
                callProcess "git" ["commit", "-qm", "base"]
                -- Modify only line 4 of M1.hs (f x = x + 10 instead of x + 1)
                writeFile "M1.hs" $ unlines
                    [ "module M1 where"
                    , ""
                    , "f :: Int -> Int"
                    , "f x = x + 10"
                    , ""
                    , "g :: Int -> Int"
                    , "g x = x * 2"
                    , ""
                    , "h :: Int -> Bool"
                    , "h x = x > 0"
                    ]
            let resF = root </> "result.txt"
                opts = (projectOpts root resF)
                    { optGitDiffBase  = Just "HEAD"
                    , optGitDiffLines = True
                    , optTestCmd      = Just "true"
                    }
            runProjectRestoring opts
            (_, _, _, total) <- readCounts resF
            total `shouldBe` 4

    it "fails with an explicit error when no project root marker exists above the scope" $
        withSystemTempDirectory "mutaskell-proj" $ \dir -> do
            let sub = dir </> "sub"
            createDirectoryIfMissing True sub
            writeFile (sub </> "M1.hs") (sampleModule "M1")
            let opts = defaultOpts { optFile = sub }
            (err, out) <- catchOutputStr (try (runProjectRestoring opts) :: IO (Either ExitCode ()))
            case err of
                Left (ExitFailure 3) -> return ()
                other                -> expectationFailure ("expected ExitFailure 3, got " ++ show other)
            out `shouldSatisfy` ("cabal.project" `isInfixOf`)
            out `shouldSatisfy` ("*.cabal" `isInfixOf`)
            out `shouldSatisfy` ("stack.yaml" `isInfixOf`)

    describe "discoverSourcesWithStats" $ do
        it "discovers each buildable source file once across overlapping roots" $
            withSystemTempDirectory "mutaskell-disc" $ \root -> do
                writeCabalProject root
                writeFiles root
                    [ ("src/A.hs", "module A where")
                    , ("src/Deep/B.hs", "module Deep.B where")
                    , ("app/Main.hs", "module Main where")
                    , ("test/Spec.hs", "module Spec where")
                    ]
                files <- withCurrentDirectory root (discoverSourcesWithStats defaultOpts)
                fst files `shouldBe`
                    ["app/Main.hs", "src/A.hs", "src/Deep/B.hs"]

        it "prunes roots contained in another root and walks each directory once" $
            withSystemTempDirectory "mutaskell-disc" $ \root -> do
                writeCabalProject root
                writeFiles root
                    [ ("src/A.hs", "module A where")
                    , ("src/Deep/B.hs", "module Deep.B where")
                    , ("app/Main.hs", "module Main where")
                    , ("test/Spec.hs", "module Spec where")
                    ]
                files <- withCurrentDirectory root (discoverSourcesWithStats defaultOpts)
                -- The package dir ("." ) subsumes the declared src/ and app/
                -- roots, so only one root is walked and no directory is listed
                -- twice.
                dsRoots (snd files) `shouldBe` 1
                dsFiles (snd files) `shouldBe` 3

        it "finds sources outside the declared source dirs via the package dir" $
            withSystemTempDirectory "mutaskell-disc" $ \root -> do
                writeFile (root </> "pkgs.cabal") $ unlines
                    [ "cabal-version: 2.4"
                    , "name: pkgs"
                    , "version: 0.1"
                    , ""
                    , "library"
                    , "    hs-source-dirs: sub/src"
                    , "    exposed-modules: A"
                    ]
                writeFiles root
                    [ ("sub/src/A.hs", "module A where")
                    , ("other/Orphan.hs", "module Orphan where")
                    ]
                files <- withCurrentDirectory root (discoverSourcesWithStats defaultOpts)
                fst files `shouldBe` ["other/Orphan.hs", "sub/src/A.hs"]

        it "discovers only files within the scope directory" $
            withSystemTempDirectory "mutaskell-disc" $ \root -> do
                writeCabalProject root
                writeFiles root
                    [ ("src/A.hs", "module A where")
                    , ("src/Deep/B.hs", "module Deep.B where")
                    , ("app/Main.hs", "module Main where")
                    , ("test/Spec.hs", "module Spec where")
                    ]
                files <- withCurrentDirectory root (discoverSourcesWithStats defaultOpts { optFile = "src" })
                fst files `shouldBe` ["src/A.hs", "src/Deep/B.hs"]

    describe "findProjectRoot" $ do
        it "finds root containing cabal.project" $
            withSystemTempDirectory "proj-root" $ \root -> do
                writeFile (root </> "cabal.project") ""
                let sub = root </> "a" </> "b"
                createDirectoryIfMissing True sub
                res <- findProjectRoot sub
                res `shouldBe` Just root

        it "finds root containing *.cabal" $
            withSystemTempDirectory "proj-root" $ \root -> do
                writeFile (root </> "mypkg.cabal") ""
                let sub = root </> "src"
                createDirectoryIfMissing True sub
                res <- findProjectRoot sub
                res `shouldBe` Just root

        it "finds root containing stack.yaml" $
            withSystemTempDirectory "proj-root" $ \root -> do
                writeFile (root </> "stack.yaml") ""
                let sub = root </> "src"
                createDirectoryIfMissing True sub
                res <- findProjectRoot sub
                res `shouldBe` Just root

        it "returns Nothing when no marker exists" $
            withSystemTempDirectory "proj-root" $ \root -> do
                let sub = root </> "nested"
                createDirectoryIfMissing True sub
                res <- findProjectRoot sub
                res `shouldBe` Nothing

    describe "restrictToShard" $
        it "keeps the shard's files in discovery order, ignoring shard order" $ do
            listF <- emptySystemTempFile "mutaskell-shard"
            writeFile listF "src/B.hs\nsrc/A.hs\nunheard/of.hs\n"
            got <- restrictToShard defaultOpts { optOnlyFiles = Just listF }
                       ["src/A.hs", "src/B.hs", "src/C.hs"]
            got `shouldBe` ["src/A.hs", "src/B.hs"]

    describe "distribute" $ do
        it "keeps every file exactly once, round-robin, in file order" $ do
            sort (concat (distribute 3 [1 .. 7 :: Int])) `shouldBe` [1 .. 7]
            distribute 2 [1 .. 5 :: Int] `shouldBe` [[1, 3, 5], [2, 4]]
            distribute 3 [1 .. 7 :: Int] `shouldBe` [[1, 4, 7], [2, 5], [3, 6]]
        it "yields n empty buckets for an empty list" $
            distribute 3 ([] :: [Int]) `shouldBe` [[], [], []]
        it "yields no buckets for a non-positive worker count" $
            distribute 0 [1 :: Int .. 5] `shouldBe` []

    describe "filterDiff" $ do
        it "returns all files when optGitDiffBase is Nothing" $ do
            files <- filterDiff defaultOpts ["A.hs", "B.hs"]
            files `shouldBe` ["A.hs", "B.hs"]

        it "returns only modified files when optGitDiffBase is set" $
            withSystemTempDirectory "mutaskell-proj" $ \root -> do
                withCurrentDirectory root $ do
                    callProcess "git" ["init", "-q"]
                    callProcess "git" ["config", "user.email", "test@example.com"]
                    callProcess "git" ["config", "user.name", "test"]
                    writeFile "A.hs" "module A where\n"
                    writeFile "B.hs" "module B where\n"
                    callProcess "git" ["add", "."]
                    callProcess "git" ["commit", "-qm", "base"]
                    writeFile "A.hs" "module A where\n-- change\n"
                    let opts = defaultOpts { optGitDiffBase = Just "HEAD" }
                    kept <- filterDiff opts ["A.hs", "B.hs"]
                    kept `shouldBe` ["A.hs"]
  where
    -- Strict read: project mode rewrites run-state files, so a lazy handle
    -- against them must not outlive this helper.
    strictRead p = readFile p >>= \s -> length s `seq` return s
    containsAll needles hay = all (`isInfixOf` hay) needles
