module Test.Mutaskell.OutputSpec where

import Test.Hspec
import App.Output (unifiedDiff)
import App.Filter (parseDiffChangedLines)
import Data.List (isInfixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

        it "produces valid hunk headers for removed lines" $ do
            let s1 = "line 1\nline 2\nline 3\n"
                s2 = "line 1\nline 2\n"
                diff = unifiedDiff s1 s2
            diff `shouldSatisfy` ("@@ -1,3 +1,2 @@\n" `isInfixOf`)

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
