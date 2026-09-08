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
    , getCurrentDirectory
    , listDirectory
    , setCurrentDirectory
    , withCurrentDirectory
    )
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (emptySystemTempFile, withSystemTempDirectory)
import Test.Hspec

import App.Orchestrator (stateDir)
import App.Opts (Opts (..), defaultOpts)
import App.Project
    ( DiscoveryStats (..)
    , discoverSourcesWithStats
    , distribute
    , restrictToShard
    , runProject
    )

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
makeProject root n =
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

-- | Read the @killed alive skipped total@ line written for the parent.
readCounts :: FilePath -> IO (Int, Int, Int, Int)
readCounts p = do
    s <- readFile p
    return $ case map read (words s) of
        [k, a, sk, t] -> (k, a, sk, t)
        _             -> error ("bad result line: " ++ s)

spec :: Spec
spec = describe "runProject (serial)" $ do
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

    it "records completed files for resume" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            let resF = root </> "result.txt"
            runProjectRestoring (projectOpts root resF)
            progress <- listDirectory (root </> stateDir)
            progress `shouldContain` ["progress"]
            done <- lines <$> strictRead (root </> stateDir </> "progress")
            done `shouldContain` ["M1.hs"]
            done `shouldContain` ["M2.hs"]
            done `shouldContain` ["M3.hs"]

    it "skips completed files on a resumed run" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            let resF = root </> "result.txt"
            runProjectRestoring (projectOpts root resF)
            (_, _, _, total) <- readCounts resF
            total `shouldSatisfy` (> 0)
            runProjectRestoring (projectOpts root resF)
            counts <- readCounts resF
            counts `shouldBe` (0, 0, 0, 0)

    it "stops at the mutant budget and leaves the rest for resume" $
        withSystemTempDirectory "mutaskell-proj" $ \root -> do
            makeProject root 3
            let resF = root </> "result.txt"
                opts = (projectOpts root resF) { optMaxMutants = Just 1 }
            runProjectRestoring opts
            (_, _, _, total) <- readCounts resF
            -- These counts derive from the configured budget, not from the
            -- mutator set, so they are stable to assert.
            total `shouldBe` 1
            done <- lines <$> strictRead (root </> stateDir </> "progress")
            length done `shouldBe` 1

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
  where
    -- Strict read: project mode rewrites run-state files, so a lazy handle
    -- against them must not outlive this helper.
    strictRead p = readFile p >>= \s -> length s `seq` return s
    containsAll needles hay = all (`isInfixOf` hay) needles
