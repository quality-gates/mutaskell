-- | Filter stages applied to generated mutants before sampling.
module Test.Mutaskell.FilterSpec where

import Control.Exception (bracket)
import Data.List (isInfixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (withCurrentDirectory)
import System.IO (IOMode (..), hClose, hFlush, stderr, withFile)
import System.IO.Temp (emptySystemTempFile, withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

import App.Filter
    ( applyAnnotations
    , applyBaseline
    , applyBlacklist
    , applyDiffLines
    , applyDisableEnable
    , applyIgnoreLines
    , applyRunMutantId
    , cacheMutantIds
    , parseAnnotations
    )
import Test.Mutaskell.Config (MuVar (..))
import Test.Mutaskell.TestAdapter (Mutant (..))
import Test.Mutaskell.Tix (toSpan)
import Test.Mutaskell.Utils.Common (hash)

mkMutant :: String -> MuVar -> Int -> Mutant
mkMutant src mtype line = Mutant
    { _mutant = src
    , _mtype  = mtype
    , _mspan  = toSpan (line, 1, line, 10)
    }

srcA, srcB, srcC :: String
srcA = "module A where\nf = 1\n"
srcB = "module B where\nf = 2\n"
srcC = "module C where\nf = 3\n"

mA, mB, mC, mFn, mOther :: Mutant
mA     = mkMutant srcA MutateValues 2
mB     = mkMutant srcB MutateValues 3
mC     = mkMutant srcC MutateValues 4
mFn    = mkMutant srcA MutateFunctions 2
mOther = mkMutant srcB (MutateOther "remove-not") 5

allMs :: [Mutant]
allMs = [mA, mB, mC, mFn, mOther]

captureStderr :: IO a -> IO (a, String)
captureStderr action = do
    path <- emptySystemTempFile "mutaskell-stderr"
    r <- withFile path WriteMode $ \h ->
        bracket (redirectStderr h) restoreStderr $ \_ -> do
            x <- action
            hFlush stderr
            return x
    s <- readFile path
    length s `seq` return (r, s)
  where
    redirectStderr h = do
        old <- hDuplicate stderr
        hDuplicateTo h stderr
        return old
    restoreStderr old = hDuplicateTo old stderr >> hClose old

writeLines :: FilePath -> [String] -> IO ()
writeLines path = writeFile path . unlines

spec :: Spec
spec = do
    describe "applyAnnotations" $ do
        it "returns the same ordered mutants when there are no annotations" $
            applyAnnotations [] allMs `shouldBe` allMs

        it "suppresses every mutator on the next line when the name list is empty" $
            applyAnnotations [(1, [])] [mA, mFn, mB] `shouldBe` [mB]

        it "suppresses only the named mutator on the next line" $
            applyAnnotations [(1, ["literal-values"])] [mA, mFn, mB]
                `shouldBe` [mFn, mB]

        it "treats overlapping annotations on the same line as a union, including suppress-all" $ do
            applyAnnotations [(1, ["literal-values"]), (1, ["functions"])] [mA, mFn, mB]
                `shouldBe` [mB]
            applyAnnotations [(1, ["literal-values"]), (1, [])] [mA, mFn, mB]
                `shouldBe` [mB]

        it "leaves mutants whose start line is out of range" $
            applyAnnotations [(10, [])] [mA, mB] `shouldBe` [mA, mB]

        it "keeps survivor order" $
            applyAnnotations [(2, ["literal-values"])] [mA, mB, mC]
                `shouldBe` [mA, mC]

        it "parses inline comments and applies them to the following line" $ do
            let src = unlines
                    [ "module M where"
                    , "-- mucheck: disable-next-line literal-values"
                    , "f = 1"
                    , "-- mucheck: disable-next-line"
                    , "g = 2"
                    ]
                anns = parseAnnotations src
                onF = mkMutant "f" MutateValues 3
                onG = mkMutant "g" MutateFunctions 5
                other = mkMutant "h" MutateValues 6
            applyAnnotations anns [onF, onG, other] `shouldBe` [other]

    describe "applyBaseline" $ do
        it "returns the same ordered mutants when no file is given" $ do
            got <- applyBaseline Nothing allMs
            got `shouldBe` allMs

        it "warns and returns every mutant when the file cannot be read" $ do
            (got, err) <- captureStderr $ applyBaseline (Just "/no/such/baseline") allMs
            got `shouldBe` allMs
            err `shouldSatisfy` ("Warning: could not read baseline file:" `isInfixOf`)

        it "drops mutants whose hash is listed, including duplicate IDs, and keeps order" $
            withSystemTempDirectory "mutaskell-baseline" $ \dir -> do
                let path = dir ++ "/baseline"
                    listed = [hash srcB, hash srcB, hash srcC]
                writeLines path ("" : listed ++ [""])
                got <- applyBaseline (Just path) allMs
                got `shouldBe` [mA, mFn]

        it "returns every mutant when the file is empty" $
            withSystemTempDirectory "mutaskell-baseline" $ \dir -> do
                let path = dir ++ "/baseline"
                writeFile path ""
                got <- applyBaseline (Just path) allMs
                got `shouldBe` allMs

    describe "applyBlacklist" $ do
        it "returns the same ordered mutants when no file is given" $ do
            got <- applyBlacklist Nothing allMs
            got `shouldBe` allMs

        it "warns and returns every mutant when the file cannot be read" $ do
            (got, err) <- captureStderr $ applyBlacklist (Just "/no/such/blacklist") allMs
            got `shouldBe` allMs
            err `shouldSatisfy` ("Warning: could not read blacklist file:" `isInfixOf`)

        it "drops mutants whose hash is listed, including duplicate IDs, and keeps order" $
            withSystemTempDirectory "mutaskell-blacklist" $ \dir -> do
                let path = dir ++ "/blacklist"
                writeLines path [hash srcA, hash srcA]
                got <- applyBlacklist (Just path) allMs
                got `shouldBe` [mB, mC, mOther]

    describe "applyDiffLines" $ do
        it "returns the same ordered mutants when the base ref is unset" $ do
            got <- applyDiffLines "X.hs" Nothing True allMs
            got `shouldBe` allMs

        it "returns the same ordered mutants when line filtering is off" $ do
            got <- applyDiffLines "X.hs" (Just "HEAD") False allMs
            got `shouldBe` allMs

        it "keeps mutants whose start line is in the changed-line set" $
            withSystemTempDirectory "mutaskell-diff" $ \dir ->
                withCurrentDirectory dir $ do
                    callProcess "git" ["init", "-q"]
                    callProcess "git" ["config", "user.email", "bench@example.com"]
                    callProcess "git" ["config", "user.name", "bench"]
                    writeFile "M.hs" "module M where\nf = 1\ng = 2\n"
                    callProcess "git" ["add", "M.hs"]
                    callProcess "git" ["commit", "-qm", "base"]
                    writeFile "M.hs" "module M where\nf = 9\ng = 2\n"
                    let onF = mkMutant "f" MutateValues 2
                        onG = mkMutant "g" MutateValues 3
                    got <- applyDiffLines "M.hs" (Just "HEAD") True [onF, onG]
                    got `shouldBe` [onF]

        it "returns every mutant when the git command fails" $ do
            (got, _) <- captureStderr $ applyDiffLines "X.hs" (Just "not-a-ref") True allMs
            got `shouldBe` allMs

    describe "applyIgnoreLines" $ do
        let src = unlines ["module M where", "f = 1 -- skip", "g = 2"]
            onSkip = mkMutant "f" MutateValues 2
            onKeep = mkMutant "g" MutateValues 3
            onBad  = mkMutant "h" MutateValues 99

        it "returns the same ordered mutants when the pattern list is empty" $
            applyIgnoreLines src [] [onSkip, onKeep] `shouldBe` [onSkip, onKeep]

        it "drops mutants whose source line contains an ordinary pattern" $
            applyIgnoreLines src ["skip"] [onSkip, onKeep] `shouldBe` [onKeep]

        it "treats an empty pattern as a match on every line, including out-of-range lines" $
            applyIgnoreLines src [""] [onSkip, onKeep, onBad] `shouldBe` []

        it "treats an out-of-range start line as empty text" $
            applyIgnoreLines src ["skip"] [onBad, onKeep] `shouldBe` [onBad, onKeep]

        it "keeps survivor order" $
            applyIgnoreLines src ["skip"] [onKeep, onSkip, mC] `shouldBe` [onKeep, mC]

    describe "applyRunMutantId" $ do
        it "returns the same ordered mutants when no ID is given" $
            applyRunMutantId Nothing allMs `shouldBe` allMs

        it "keeps only the mutant whose hash matches" $
            applyRunMutantId (Just (hash srcB)) allMs `shouldBe` [mB, mOther]

        it "returns no mutants when the ID is unknown" $
            applyRunMutantId (Just "no-such-id") allMs `shouldBe` []

        it "computes the ID from the current source, not a stale identity" $ do
            let updated = mA { _mutant = srcC }
            applyRunMutantId (Just (hash srcC)) [updated] `shouldBe` [updated]
            applyRunMutantId (Just (hash srcA)) [updated] `shouldBe` []
            hash (_mutant updated) `shouldBe` hash srcC
            map snd (cacheMutantIds [updated]) `shouldBe` [hash srcC]

    describe "hash" $ do
        it "differs for distinct sources used in these fixtures" $ do
            hash srcA `shouldNotBe` hash srcB
            hash srcB `shouldNotBe` hash srcC

    describe "filter chain" $ do
        it "preserves order through empty stages" $ do
            let anns = [] :: [(Int, [String])]
            got0 <- applyBaseline Nothing $
                    applyAnnotations anns $
                    applyDisableEnable [] [] allMs
            got1 <- applyBlacklist Nothing got0
            got2 <- applyDiffLines "X.hs" Nothing False got1
            let got3 = applyIgnoreLines "src" [] got2
                got4 = applyRunMutantId Nothing got3
            got4 `shouldBe` allMs

        it "applies populated stages in pipeline order and keeps survivors ordered" $
            withSystemTempDirectory "mutaskell-chain" $ \dir -> do
                let baseP = dir ++ "/baseline"
                    blackP = dir ++ "/blacklist"
                    src = unlines
                        [ "module M where"
                        , "f = 1 -- generated"
                        , "g = 2"
                        , "h = 3"
                        ]
                    onFval = mkMutant srcA MutateValues 2
                    onFfn  = mkMutant srcA MutateFunctions 2
                    onG    = mkMutant srcB MutateValues 3
                    onH    = mkMutant srcC MutateValues 4
                    onStar = mkMutant "wild" (MutateOther "case-alt-remove") 4
                    ms     = [onFval, onFfn, onG, onH, onStar]
                writeLines baseP [hash srcC]
                writeLines blackP [hash srcB]
                let afterEnable = applyDisableEnable [] ["literal-values", "other:*"] ms
                    afterAnns   = applyAnnotations [(1, ["literal-values"])] afterEnable
                afterBase <- applyBaseline (Just baseP) afterAnns
                afterBlack <- applyBlacklist (Just blackP) afterBase
                afterDiff <- applyDiffLines "X.hs" Nothing False afterBlack
                let afterIgnore = applyIgnoreLines src ["generated"] afterDiff
                    afterId     = applyRunMutantId Nothing afterIgnore
                afterEnable `shouldBe` [onFval, onG, onH, onStar]
                afterAnns   `shouldBe` [onG, onH, onStar]
                afterBase   `shouldBe` [onG, onStar]
                afterBlack  `shouldBe` [onStar]
                afterDiff   `shouldBe` [onStar]
                afterIgnore `shouldBe` [onStar]
                afterId     `shouldBe` [onStar]
