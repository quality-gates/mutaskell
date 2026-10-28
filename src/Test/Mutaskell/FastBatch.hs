{- | Evaluate a module of boolean-literal mutants in one GHC process.

The ordinary path rebuilds and interprets each mutant on its own. For a
module whose mutants are only @True@/@False@ flips, each mutant is that same
module with one token changed. This module builds one program that takes the
mutant number as an argument, compiles it once, and reads one result line per
mutant.

The function returns 'Nothing' when the module is not of that shape, or when
the batch compiler fails. The caller then uses the ordinary interpreter.
-}
module Test.Mutaskell.FastBatch
    ( tryFastBatch
    ) where

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isInfixOf, isPrefixOf, sortOn)
import qualified Data.Set as Set
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import Test.Mutaskell.AnalysisSummary (MAnalysisSummary)
import Test.Mutaskell.Config (MuVar (..), defaultConfig)
import Test.Mutaskell.Interpreter
    ( MutantSummary (..)
    , findPkgEnvArgs
    , summaryFromMutantSummaries
    )
import Test.Mutaskell.MuOp (Module_, MuOp, getSpan)
import Test.Mutaskell.Mutation
    ( applicableOpsWith
    , autoDiscoverTestNames
    , buildSelectorMetadata
    , getASTFromStr
    , getAnn
    , putDecl
    , splitAnnotationsWith
    )
import Test.Mutaskell.TestAdapter (toMutant)
import Test.Mutaskell.Tix (toSpan)

-- | Run the fast batch, or report that this module must use the interpreter.
--
-- The 'Int' is the maximum number of mutants. A larger set is refused so the
-- ordinary sampler stays in charge of the cut.
tryFastBatch :: Int -> String -> IO (Maybe (MAnalysisSummary, [MutantSummary]))
tryFastBatch maxN src = do
    parsed <- getASTFromStr src
    case parsed of
        Left _ -> return Nothing
        Right ast -> case prepareBatch maxN src ast of
            Nothing -> return Nothing
            Just (pairs, program) -> do
                compiled <- compileAndRun program
                return $ do
                    marks <- compiled
                    if length marks /= length pairs
                        then Nothing
                        else
                            let summaries = zipWith toSummary pairs marks
                            in Just (summaryFromMutantSummaries summaries, summaries)
  where
    toSummary (mutantSrc, sp) mark =
        let mutant = toMutant (MutateValues, toSpan sp, mutantSrc)
        in case mark of
            Killed -> MSumKilled mutant []
            Alive -> MSumAlive mutant []

-- | One result line from the batch program.
data Mark = Killed | Alive
    deriving (Eq, Show)

-- | A boolean flip in the original source.
--
-- Columns are 1-based. The end column is exclusive, matching 'getSpan'.
data Site = Site
    { siteLine :: Int
    , siteCol :: Int
    , siteEnd :: Int
    , siteOld :: String
    , siteNew :: String
    , siteId :: Int
    }

data Simple = Simple
    { sName :: String
    , sRhs :: String
    , sLine :: Int
    }

-- | Build mutant sources and the batch program.
prepareBatch ::
    Int ->
    String ->
    Module_ ->
    Maybe ([(String, (Int, Int, Int, Int))], String)
prepareBatch maxN src ast = do
    let metadata = buildSelectorMetadata ast
        (_, keptDecls) = splitAnnotationsWith metadata ast
        ops = map snd (applicableOpsWith metadata defaultConfig (putDecl ast keptDecls))
    raw <- mapM (boolSite src) ops
    let sites = zipWith (\s i -> s {siteId = i}) (sortOn siteKey raw) [1 ..]
    if null sites || length sites > maxN
        then Nothing
        else do
            pairs <- mapM (\s -> fmap (\m -> (m, siteSpan s)) (patchSource src s)) sites
            tests <- testNames ast
            program <- renderProgram src sites tests
            return (pairs, program)
  where
    siteKey s = (siteLine s, siteCol s)
    siteSpan s = (siteLine s, siteCol s, siteLine s, siteEnd s)

testNames :: Module_ -> Maybe [String]
testNames ast =
    let names = nubKeep (getAnn ast "Test" ++ autoDiscoverTestNames ast)
    in if null names then Nothing else Just names

nubKeep :: Ord a => [a] -> [a]
nubKeep = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert x seen) xs

-- | Accept an operator only when it flips a @True@ or @False@ token in place.
boolSite :: String -> MuOp -> Maybe Site
boolSite src op
    | l1 /= l2 || l1 <= 0 || c1 <= 0 || c2 <= c1 = Nothing
    | otherwise = do
        sl <- lineSlice src l1 c1 c2
        (before, after) <- splitShow (show op)
        let old = filter (not . isSpace) before
            new = filter (not . isSpace) after
        if sl == old && isBool old && isBool new && old /= new
            then
                Just
                    Site
                        { siteLine = l1
                        , siteCol = c1
                        , siteEnd = c2
                        , siteOld = old
                        , siteNew = new
                        , siteId = 0
                        }
            else Nothing
  where
    (l1, c1, l2, c2) = getSpan op
    isBool t = t == "True" || t == "False"

-- | 'Show' for 'MuOp' is @{ before } ==> { after }@.
splitShow :: String -> Maybe (String, String)
splitShow s = do
    (pre, post) <- breakOn "\n} ==> {\n" s
    before <- dropBrace pre
    let after = take (length post - length "\n}") post
    return (before, after)
  where
    dropBrace ('{' : '\n' : rest) = Just rest
    dropBrace _ = Nothing

breakOn :: String -> String -> Maybe (String, String)
breakOn needle hay = go 0
  where
    n = length needle
    go i
        | i + n > length hay = Nothing
        | take n (drop i hay) == needle = Just (take i hay, drop (i + n) hay)
        | otherwise = go (i + 1)

lineSlice :: String -> Int -> Int -> Int -> Maybe String
lineSlice src line c1 c2 = do
    row <- safeIndex (lines src) (line - 1)
    if c2 - 1 > length row
        then Nothing
        else Just (take (c2 - c1) (drop (c1 - 1) row))

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
    | i < 0 = Nothing
    | otherwise = case drop i xs of
        (x : _) -> Just x
        [] -> Nothing

-- | One mutant source: the original module with this site's token replaced.
patchSource :: String -> Site -> Maybe String
patchSource src site = do
    let ls = lines src
    row <- safeIndex ls (siteLine site - 1)
    row' <- replaceCols row (siteCol site) (siteEnd site) (siteNew site)
    return (unlines (replaceAt ls (siteLine site - 1) row'))

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i x =
    let (pre, rest) = splitAt i xs
    in case rest of
        (_ : post) -> pre ++ (x : post)
        [] -> xs

replaceCols :: String -> Int -> Int -> String -> Maybe String
replaceCols row c1 c2 new
    | c1 < 1 || c2 < c1 || c2 - 1 > length row = Nothing
    | otherwise = Just (take (c1 - 1) row ++ new ++ drop (c2 - 1) row)

-- | Render a @Main@ module.
-- Returns 'Nothing' when a declaration is not a simple equation.
renderProgram :: String -> [Site] -> [String] -> Maybe String
renderProgram src sites tests
    | "__mut" `isInfixOf` src = Nothing
    | any reserved (map sName binds) = Nothing
    | any (`notElem` map sName binds) tests = Nothing
    | otherwise = do
        mapM_ accept numbered
        body <- mapM (emitLine names sites) numbered
        let kept = [line | line <- body, not (null line), not (isPragma line)]
            driver =
                [ "main :: IO ()"
                , "main = mapM_ __mutaskellReport [1 .. " ++ show (length sites) ++ " :: Int]"
                , "  where"
                , "    __mutaskellReport __mut = putStrLn (if " ++ killedExpr tests names ++ " then \"K\" else \"A\")"
                ]
        return (unlines (pragmas ++ ["module Main where"] ++ kept ++ driver))
  where
    numbered = zip [1 ..] (lines src)
    binds = [Simple name rhs n | (n, line) <- numbered, Just (name, rhs) <- [splitEquation line]]
    seeds = [sName b | b <- binds, any (\s -> siteLine s == sLine b) sites]
    names = paramNames seeds binds
    pragmas = filter isPragma (lines src)
    reserved name = name == "main" || name == "__mutaskellReport"
    accept (_, line)
        | isSkippable line || isEquation line = Just ()
        | otherwise = Nothing

paramNames :: [String] -> [Simple] -> Set.Set String
paramNames seeds binds = grow (Set.fromList seeds)
  where
    grow s
        | s == next = s
        | otherwise = grow next
      where
        next = Set.union s (Set.fromList [sName b | b <- binds, mentions s (sRhs b)])

mentions :: Set.Set String -> String -> Bool
mentions names rhs = any (`Set.member` names) (idents rhs)

idents :: String -> [String]
idents [] = []
idents (c : cs)
    | isIdent c =
        let (w, rest) = span isIdent (c : cs)
        in w : idents rest
    | otherwise = idents cs

emitLine :: Set.Set String -> [Site] -> (Int, String) -> Maybe String
emitLine names sites (n, line)
    | isPragma line = Just ""
    | "module " `isPrefixOf` dropWhile isSpace line = Just ""
    | isSkippable line = Just line
    | otherwise = do
        patched <- foldM (flip patchLine) line rightFirst
        (name, rhs) <- splitEquation patched
        let rhs' = applyParams (Set.delete name names) rhs
        if name `Set.member` names
            then Just (name ++ " __mut = " ++ rhs')
            else Just (name ++ " = " ++ rhs')
  where
    rightFirst = sortOn (negate . siteCol) [s | s <- sites, siteLine s == n]

patchLine :: Site -> String -> Maybe String
patchLine site line = replaceCols line (siteCol site) (siteEnd site) replacement
  where
    replacement =
        "(if __mut == "
            ++ show (siteId site)
            ++ " then "
            ++ siteNew site
            ++ " else "
            ++ siteOld site
            ++ ")"

applyParams :: Set.Set String -> String -> String
applyParams names = go
  where
    go [] = []
    go (c : cs)
        | isIdent c =
            let (w, rest) = span isIdent (c : cs)
            in (if w `Set.member` names then "(" ++ w ++ " __mut)" else w) ++ go rest
        | otherwise = c : go cs

killedExpr :: [String] -> Set.Set String -> String
killedExpr tests names = foldr1 (\a b -> a ++ " || " ++ b) (map one tests)
  where
    one t
        | t `Set.member` names = "show (" ++ t ++ " __mut) == \"AssertFailure\""
        | otherwise = "show " ++ t ++ " == \"AssertFailure\""

isPragma :: String -> Bool
isPragma line = "{-#" `isPrefixOf` dropWhile isSpace line

isSkippable :: String -> Bool
isSkippable line =
    let t = dropWhile isSpace line
    in null t
        || "--" `isPrefixOf` t
        || isPragma line
        || "import " `isPrefixOf` t
        || "import\t" `isPrefixOf` t
        || t == "import"
        || "module " `isPrefixOf` t

isEquation :: String -> Bool
isEquation line = case splitEquation line of
    Just _ -> True
    Nothing -> False

splitEquation :: String -> Maybe (String, String)
splitEquation line = case break (== '=') line of
    (lhs, '=' : rhs) -> do
        name <- simpleName (trim lhs)
        return (name, dropWhile isSpace rhs)
    _ -> Nothing

simpleName :: String -> Maybe String
simpleName s
    | null s = Nothing
    | all isIdent s && isIdentStart (head s) = Just s
    | otherwise = Nothing

isIdentStart :: Char -> Bool
isIdentStart c = isAlphaNum c || c == '_'

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '\''

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

compileAndRun :: String -> IO (Maybe [Mark])
compileAndRun program =
    withSystemTempDirectory "mutaskell-fast-batch" $ \dir -> do
        let srcPath = dir ++ "/Main.hs"
        writeFile srcPath program
        pkg <- findPkgEnvArgs
        (code, out, _err) <-
            readProcessWithExitCode
                "ghc"
                (["-dynamic", "-outputdir", dir, "-e", "main", srcPath] ++ pkg)
                ""
        return $ case code of
            ExitSuccess -> parseMarks out
            ExitFailure _ -> Nothing

parseMarks :: String -> Maybe [Mark]
parseMarks out = mapM mark (filter (not . null) (lines out))
  where
    mark "K" = Just Killed
    mark "A" = Just Alive
    mark _ = Nothing
