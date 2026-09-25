module Test.Mutaskell.CoverageIndexSpec where

import Test.Hspec
import Test.Mutaskell.CoverageIndex
    ( CoverageIndex
    , emptyCoverageIndex
    , fromModuleResults
    , fromModuleSpans
    , fromSpans
    , isCovered
    , lookupModuleSpans
    , toSpan
    , uncoveredSpans
    )

spec :: Spec
spec = describe "Test.Mutaskell.CoverageIndex" $ do
    describe "emptyCoverageIndex" $ do
        it "treats all spans as covered" $ do
            let sp = toSpan (1, 1, 1, 10)
            isCovered emptyCoverageIndex "Any" sp `shouldBe` True

        it "returns empty uncovered spans" $ do
            uncoveredSpans emptyCoverageIndex "Any" `shouldBe` []

        it "returns Right Nothing on module lookup" $ do
            lookupModuleSpans emptyCoverageIndex "Any" `shouldBe` Right Nothing

    describe "fromSpans (single module / anonymous coverage)" $ do
        let uncov = [toSpan (2, 1, 2, 10)]
            idx = fromSpans uncov

        it "reports spans inside uncovered as not covered" $ do
            let inner = toSpan (2, 3, 2, 8)
            isCovered idx "Mod" inner `shouldBe` False
            isCovered idx "" inner `shouldBe` False

        it "reports spans outside uncovered as covered" $ do
            let outer = toSpan (3, 1, 3, 5)
            isCovered idx "Mod" outer `shouldBe` True
            isCovered idx "" outer `shouldBe` True

        it "returns the uncovered spans list" $ do
            uncoveredSpans idx "Mod" `shouldBe` uncov
            uncoveredSpans idx "" `shouldBe` uncov

    describe "fromModuleSpans (multi-module coverage)" $ do
        let sp1 = toSpan (2, 1, 2, 10)
            sp2 = toSpan (5, 1, 5, 20)
            idx = fromModuleSpans
                [ ("pkg/ModuleA", [sp1])
                , ("pkg/ModuleB", [sp2])
                ]

        it "resolves modules by exact qualified name" $ do
            lookupModuleSpans idx "pkg/ModuleA" `shouldBe` Right (Just [sp1])
            uncoveredSpans idx "pkg/ModuleA" `shouldBe` [sp1]

        it "resolves modules by unqualified suffix name" $ do
            lookupModuleSpans idx "ModuleA" `shouldBe` Right (Just [sp1])
            uncoveredSpans idx "ModuleA" `shouldBe` [sp1]

        it "returns Right Nothing for unmatched modules" $ do
            lookupModuleSpans idx "ModuleC" `shouldBe` Right Nothing
            uncoveredSpans idx "ModuleC" `shouldBe` []
            isCovered idx "ModuleC" sp1 `shouldBe` True

        it "evaluates containment correctly for matched modules" $ do
            isCovered idx "ModuleA" (toSpan (2, 3, 2, 5)) `shouldBe` False
            isCovered idx "ModuleA" (toSpan (3, 1, 3, 5)) `shouldBe` True
            isCovered idx "ModuleB" (toSpan (5, 5, 5, 10)) `shouldBe` False
            isCovered idx "ModuleB" (toSpan (2, 1, 2, 10)) `shouldBe` True

        it "does not gate an ambiguous unqualified module name" $ do
            let ambiguousIdx = fromModuleSpans
                    [ ("pkg1/Shared", [sp1])
                    , ("pkg2/Shared", [sp2])
                    ]
            lookupModuleSpans ambiguousIdx "Shared" `shouldBe` Right Nothing
            uncoveredSpans ambiguousIdx "Shared" `shouldBe` []
            isCovered ambiguousIdx "Shared" sp1 `shouldBe` True
            -- Qualified names still resolve uniquely
            lookupModuleSpans ambiguousIdx "pkg1/Shared" `shouldBe` Right (Just [sp1])
            lookupModuleSpans ambiguousIdx "pkg2/Shared" `shouldBe` Right (Just [sp2])

    describe "fromModuleResults with errors" $ do
        let errIdx = fromModuleResults
                [ ("BadModule", Left "Coverage error: cannot find BadModule.mix")
                , ("GoodModule", Right [toSpan (1, 1, 1, 10)])
                ]

        it "propagates error on lookup for failed modules" $ do
            lookupModuleSpans errIdx "BadModule" `shouldBe`
                Left "Coverage error: cannot find BadModule.mix"

        it "does not gate mutants in errored modules" $ do
            isCovered errIdx "BadModule" (toSpan (1, 1, 1, 5)) `shouldBe` True

        it "still resolves healthy modules" $ do
            lookupModuleSpans errIdx "GoodModule" `shouldBe`
                Right (Just [toSpan (1, 1, 1, 10)])
            isCovered errIdx "GoodModule" (toSpan (1, 2, 1, 5)) `shouldBe` False

    describe "in-memory index reuse" $ do
        it "resolves multiple distinct module queries deterministically from one index" $ do
            let m1Spans = [toSpan (1, 1, 1, 10)]
                m2Spans = [toSpan (2, 1, 2, 10)]
                m3Spans = [toSpan (3, 1, 3, 10)]
                sharedIdx = fromModuleSpans
                    [ ("M1", m1Spans)
                    , ("M2", m2Spans)
                    , ("M3", m3Spans)
                    ]
            lookupModuleSpans sharedIdx "M1" `shouldBe` Right (Just m1Spans)
            lookupModuleSpans sharedIdx "M2" `shouldBe` Right (Just m2Spans)
            lookupModuleSpans sharedIdx "M3" `shouldBe` Right (Just m3Spans)
            uncoveredSpans sharedIdx "M1" `shouldBe` m1Spans
            uncoveredSpans sharedIdx "M2" `shouldBe` m2Spans
            uncoveredSpans sharedIdx "M3" `shouldBe` m3Spans
