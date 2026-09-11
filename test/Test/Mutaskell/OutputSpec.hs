module Test.Mutaskell.OutputSpec where

import Data.Aeson (FromJSON(..), (.:), withObject)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import Test.Hspec
import App.Output
    ( buildHtmlReportWithDiffs
    , groupConsec
    , prepareMutantDiffs
    , unifiedDiff
    ,     writeAgenticJsonLoggerWithDiffs
    , writeGitlabLogger
    , writeHtmlLoggerWithDiffs
    , writeJsonLogger
    )
import App.Opts (Opts(..), defaultOpts)
import App.Filter (parseDiffChangedLines)
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary(..))
import Test.Mutaskell.Config (MuVar(..))
import Test.Mutaskell.Interpreter (MutantSummary(..))
import Test.Mutaskell.TestAdapter (Mutant(..))
import Test.Mutaskell.Tix (toSpan)
import Data.List (isInfixOf)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

data AgenticDocument = AgenticDocument [AgenticMutant]
    deriving (Eq, Show)

data AgenticMutant = AgenticMutant
    { agenticContext :: String
    , agenticDiff :: String
    }
    deriving (Eq, Show)

instance FromJSON AgenticDocument where
    parseJSON = withObject "AgenticDocument" $ \obj ->
        AgenticDocument <$> obj .: Key.fromString "mutants"

instance FromJSON AgenticMutant where
    parseJSON = withObject "AgenticMutant" $ \obj ->
        AgenticMutant <$> obj .: Key.fromString "context"
                      <*> obj .: Key.fromString "diff"

data GitlabEntry = GitlabEntry String String
    deriving (Eq, Show)

instance FromJSON GitlabEntry where
    parseJSON = withObject "GitlabEntry" $ \obj -> do
        description <- obj .: Key.fromString "description"
        location <- obj .: Key.fromString "location"
        path <- location .: Key.fromString "path"
        return (GitlabEntry description path)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "JSON loggers" $ do
        it "encodes agentic JSON Unicode and context newlines exactly once" $
            withSystemTempDirectory "mutaskell-output" $ \dir -> do
                let original = "line —\n"
                    mutant = Mutant "changed —\n" MutateValues (toSpan (1, 1, 1, 1))
                    summary = MSumAlive mutant []
                    analysis = MAnalysisSummary (-1) 1 1 0 0 0
                    sourcePath = dir </> "Fixture.hs"
                    outputPath = dir </> "agentic.json"
                    opts = defaultOpts
                        { optLoggerAgenticJson = Just outputPath }
                writeFile sourcePath original
                source <- readFile sourcePath
                writeAgenticJsonLoggerWithDiffs opts sourcePath source
                    (prepareMutantDiffs source [summary]) analysis
                parsed <- A.eitherDecodeFileStrict' outputPath
                document <- case parsed of
                    Left err -> expectationFailure ("invalid agentic JSON: " ++ err)
                               >> return (AgenticDocument [])
                    Right value -> return value
                document `shouldBe`
                    AgenticDocument [AgenticMutant "    1: line —\n" "@@ -1,1 +1,1 @@\n-line —\n+changed —\n"]

        it "encodes GitLab JSON Unicode in paths and descriptions" $
            withSystemTempDirectory "mutaskell-output" $ \dir -> do
                let source = "module Fixture where\n-- π\n"
                    sourcePath = dir </> "Fixture.hs"
                    targetPath = dir </> "Fixture—.hs"
                    outputPath = dir </> "gitlab.json"
                    mutant = Mutant source (MutateOther "custom—mutator")
                        (toSpan (1, 1, 1, 1))
                    opts = defaultOpts
                        { optLoggerGitlab = Just outputPath }
                writeFile sourcePath source
                writeGitlabLogger opts targetPath [MSumAlive mutant []]
                parsed <- A.eitherDecodeFileStrict' outputPath
                document <- case parsed of
                    Left err -> expectationFailure ("invalid GitLab JSON: " ++ err)
                               >> return []
                    Right value -> return value
                document `shouldBe`
                    [GitlabEntry "Mutant survived: other:custom—mutator" targetPath]

    describe "groupConsec" $ do
        it "groups ordered positions without changing their order" $ do
            groupConsec [1, 2, 3, 6, 7, 10] `shouldBe`
                [[1, 2, 3], [6, 7], [10]]

    describe "writeJsonLogger" $ do
        it "emits covered_code_msi 0.0 when coverage is present but covers zero mutants" $
            withSystemTempDirectory "mutaskell-output" $ \dir -> do
                let analysis = MAnalysisSummary 0 10 0 10 0 0
                    opts = defaultOpts { optLoggerJson = Just (dir </> "summary.json") }
                writeJsonLogger opts analysis
                json <- readFile (dir </> "summary.json")
                json `shouldSatisfy` ("\"covered_code_msi\": 0.0" `isInfixOf`)

        it "emits covered_code_msi null when coverage is absent" $
            withSystemTempDirectory "mutaskell-output" $ \dir -> do
                let analysis = MAnalysisSummary (-1) 10 0 10 0 0
                    opts = defaultOpts { optLoggerJson = Just (dir </> "summary.json") }
                writeJsonLogger opts analysis
                json <- readFile (dir </> "summary.json")
                json `shouldSatisfy` ("\"covered_code_msi\": null" `isInfixOf`)

    describe "prepared report diffs" $ do
        it "feeds the same prepared diff to agentic JSON and HTML reports" $
            withSystemTempDirectory "mutaskell-output" $ \dir -> do
                let original = "line 1\n"
                    mutant = Mutant "changed\n" MutateValues (toSpan (1, 1, 1, 1))
                    summary = MSumAlive mutant []
                    analysis = MAnalysisSummary (-1) 1 1 0 0 0
                    diffs = prepareMutantDiffs original [summary]
                    opts = defaultOpts
                        { optLoggerAgenticJson = Just (dir </> "agentic.json")
                        , optLoggerHtml = Just (dir </> "report.html")
                        }
                    expected = "@@ -1,1 +1,1 @@"
                writeAgenticJsonLoggerWithDiffs opts "Fixture.hs" original diffs analysis
                writeHtmlLoggerWithDiffs opts "Fixture.hs" original diffs analysis
                agentic <- readFile (dir </> "agentic.json")
                html <- readFile (dir </> "report.html")
                agentic `shouldSatisfy` (expected `isInfixOf`)
                html `shouldSatisfy` (expected `isInfixOf`)

        it "keeps the prepared diff path available to pure HTML rendering" $ do
            let original = "line 1\n"
                mutant = Mutant "changed\n" MutateValues (toSpan (1, 1, 1, 1))
                summary = MSumAlive mutant []
                analysis = MAnalysisSummary (-1) 1 1 0 0 0
                diffs = prepareMutantDiffs original [summary]
            buildHtmlReportWithDiffs "Fixture.hs" original diffs analysis
                `shouldSatisfy` ("@@ -1,1 +1,1 @@" `isInfixOf`)

    describe "unifiedDiff" $ do
        it "returns empty string when inputs are identical" $ do
            unifiedDiff "abc\n" "abc\n" `shouldBe` ""

        it "produces standard hunk headers with + addition range" $ do
            let s1 = "line 1\nline 2\nline 3\n"
                s2 = "line 1\nline TWO\nline 3\n"
                diff = unifiedDiff s1 s2
            diff `shouldSatisfy` (\d -> "@@ -" `isInfixOf` d && " +" `isInfixOf` d)
            diff `shouldSatisfy` ("@@ -1,3 +1,3 @@\n" `isInfixOf`)

        it "produces valid hunk headers for added lines" $ do
            let s1 = "line 1\nline 2\n"
                s2 = "line 1\nline 2\nline 3\n"
                diff = unifiedDiff s1 s2
            diff `shouldSatisfy` ("@@ -1,2 +1,3 @@\n" `isInfixOf`)

        it "reports an inserted empty line into an empty source" $ do
            unifiedDiff "" "\n" `shouldBe` "@@ -0,0 +1,1 @@\n+\n"

        it "reports a deleted empty line from a one-line source" $ do
            unifiedDiff "\n" "" `shouldBe` "@@ -1,1 +0,0 @@\n-\n"

        it "produces valid hunk headers for removed lines" $ do
            let s1 = "line 1\nline 2\nline 3\n"
                s2 = "line 1\nline 2\n"
                diff = unifiedDiff s1 s2
            diff `shouldSatisfy` ("@@ -1,3 +1,2 @@\n" `isInfixOf`)

        it "reports an inserted empty line at the end" $ do
            let diff = unifiedDiff "keep\n" "keep\n\n"
            diff `shouldBe` "@@ -1,1 +1,2 @@\n keep\n+\n"

        it "reports a deleted empty line at the end" $ do
            let diff = unifiedDiff "keep\n\n" "keep\n"
            diff `shouldBe` "@@ -1,2 +1,1 @@\n keep\n-\n"

        it "keeps adjacent changes in one contextual hunk" $ do
            let original = unlines ["line " ++ show i | i <- [1..8] :: [Int]]
                mutated = unlines [ if i == 3 then "changed 3"
                                    else if i == 4 then "changed 4"
                                    else "line " ++ show i
                                  | i <- [1..8] :: [Int] ]
            unifiedDiff original mutated `shouldBe`
                "@@ -1,6 +1,6 @@\n line 1\n line 2\n-line 3\n+changed 3\n-line 4\n+changed 4\n line 5\n line 6\n"

        it "produces multiple hunks for separated changes" $ do
            let s1 = unlines [ "l" ++ show i | i <- [1..30] :: [Int] ]
                s2 = unlines [ if i == 2 then "MOD2" else if i == 28 then "MOD28" else "l" ++ show i | i <- [1..30] :: [Int] ]
                diff = unifiedDiff s1 s2
            diff `shouldSatisfy` ("@@ -1,4 +1,4 @@\n" `isInfixOf`)
            diff `shouldSatisfy` ("@@ -26,5 +26,5 @@\n" `isInfixOf`)

    describe "parseDiffChangedLines" $ do
        it "roundtrips with unifiedDiff for modified lines" $ do
            let s1 = "line 1\nline 2\nline 3\n"
                s2 = "line 1\nline TWO\nline 3\n"
                diff = unifiedDiff s1 s2
            parseDiffChangedLines diff `shouldBe` [2]

        it "roundtrips with unifiedDiff for added lines" $ do
            let s1 = "line 1\nline 2\n"
                s2 = "line 1\nline 2\nline 3\n"
                diff = unifiedDiff s1 s2
            parseDiffChangedLines diff `shouldBe` [3]

        it "roundtrips an inserted empty line at the end" $ do
            let diff = unifiedDiff "keep\n" "keep\n\n"
            parseDiffChangedLines diff `shouldBe` [2]

        it "roundtrips with unifiedDiff for removed lines" $ do
            let s1 = "line 1\nline 2\nline 3\n"
                s2 = "line 1\nline 2\n"
                diff = unifiedDiff s1 s2
            parseDiffChangedLines diff `shouldBe` []

        it "roundtrips with unifiedDiff across multiple hunks" $ do
            let s1 = unlines [ "l" ++ show i | i <- [1..30] :: [Int] ]
                s2 = unlines [ if i == 2 then "MOD2" else if i == 28 then "MOD28" else "l" ++ show i | i <- [1..30] :: [Int] ]
                diff = unifiedDiff s1 s2
            parseDiffChangedLines diff `shouldBe` [2, 28]

        it "parses git diff --unified=0 style without count" $ do
            let diff = unlines
                    [ "--- a/Foo.hs"
                    , "+++ b/Foo.hs"
                    , "@@ -5 +5 @@"
                    , "-old"
                    , "+new"
                    ]
            parseDiffChangedLines diff `shouldBe` [5]

        it "parses git diff --unified=0 style with count" $ do
            let diff = unlines
                    [ "--- a/Foo.hs"
                    , "+++ b/Foo.hs"
                    , "@@ -10,1 +10,2 @@"
                    , "-old"
                    , "+new1"
                    , "+new2"
                    ]
            parseDiffChangedLines diff `shouldBe` [10, 11]

        it "returns empty list for pure deletion in git diff" $ do
            let diff = unlines
                    [ "--- a/Foo.hs"
                    , "+++ b/Foo.hs"
                    , "@@ -5,1 +4,0 @@"
                    , "-old"
                    ]
            parseDiffChangedLines diff `shouldBe` []

        it "falls back to header range when diff has no body lines" $ do
            let diff = "@@ -1,2 +5,3 @@\n"
            parseDiffChangedLines diff `shouldBe` [5, 6, 7]
