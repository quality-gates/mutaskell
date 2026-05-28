{-# LANGUAGE DeriveDataTypeable #-}

module Test.MuCheck.Utils.SybSpec where

import Control.Monad (MonadPlus, mzero)
import Data.Data (Data)
import Data.Generics (mkMp)
import Data.Typeable (Typeable)
import Test.Hspec
import qualified Test.MuCheck.Utils.Syb as S

-- Simple binary-tree type for testing generic traversal without depending on
-- GHC's AST types.  'isSkippedDecl' never fires for this type, so we test
-- the core once-per-site semantics in isolation.
data T = Leaf Int | Node T T
    deriving (Show, Eq, Data, Typeable)

-- Replace 1 with 2; fail on everything else.
replOne :: MonadPlus m => Int -> m Int
replOne 1 = return 2
replOne _ = mzero

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "once" $ do
        it "applies at the first matching site" $
            (S.once (mkMp replOne) (Node (Leaf 1) (Leaf 0)) :: Maybe T)
                `shouldBe` Just (Node (Leaf 2) (Leaf 0))
        it "applies at exactly one site when multiple match" $
            (S.once (mkMp replOne) (Node (Leaf 1) (Leaf 1)) :: Maybe T)
                `shouldBe` Just (Node (Leaf 2) (Leaf 1))
        it "returns Nothing when no match exists" $
            (S.once (mkMp replOne) (Node (Leaf 0) (Leaf 0)) :: Maybe T)
                `shouldBe` Nothing
        it "returns all single-site applications in the list monad" $
            (S.once (mkMp replOne) (Node (Leaf 1) (Leaf 1)) :: [T])
                `shouldBe` [Node (Leaf 2) (Leaf 1), Node (Leaf 1) (Leaf 2)]
