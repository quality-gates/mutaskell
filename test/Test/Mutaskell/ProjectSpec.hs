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
import Data.List (isInfixOf)
import System.Directory
    ( getCurrentDirectory
    , listDirectory
    , setCurrentDirectory
    )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import App.Orchestrator (stateDir)
import App.Opts (Opts (..), defaultOpts)
import App.Project (runProject)

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
  where
    -- Strict read: project mode rewrites run-state files, so a lazy handle
    -- against them must not outlive this helper.
    strictRead p = readFile p >>= \s -> length s `seq` return s
    containsAll needles hay = all (`isInfixOf` hay) needles