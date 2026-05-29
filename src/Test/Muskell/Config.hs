-- | Configuration module
module Test.Muskell.Config where

import Data.List (isPrefixOf, stripPrefix)

{- | For function mutations, whether the function is a symbol or an identifier
for example,`head` is an identifier while `==` is a symbol.
-}
data FnType = FnSymbol | FnIdent
    deriving (Eq, Show, Read)

{- | User defined function groups. Indicate whether the functions are symbols
or identifiers, and also the group of functions to interchange for.
We dont allow mixing of identifiers and functions for now (harder to
match)
-}
data FnOp = FnOp {_type :: FnType, _fns :: [String]}
    deriving (Eq, Show, Read)

-- | predicates ["pred", "id", "succ"]
predNums :: [String]
predNums = ["pred", "id", "succ"]

-- | functions on lists ["sum", "product", "maximum", "minimum", "head", "last"]
arithLists :: [String]
arithLists = ["sum", "product", "maximum", "minimum", "head", "last"]

-- | comparison operators ["<", ">", "<=", ">=", "/=", "=="]
comparators :: [String]
comparators = ["<", ">", "<=", ">=", "/=", "=="]

-- | binary arithmetic ["+", "-", "*", "/"]
binAriths :: [String]
binAriths = ["+", "-", "*", "/"]

-- | logical operators ["&&", "||"]
logicOps :: [String]
logicOps = ["&&", "||"]

-- | fold functions ["foldl", "foldl'", "foldr"]
foldFns :: [String]
foldFns = ["foldl", "foldl'", "foldr"]

-- | Data.Bits symbols [".&.", ".|."]
bitSymbols :: [String]
bitSymbols = [".&.", ".|."]

-- | Data.Bits identifiers ["xor", "shiftL", "shiftR", "complement"]
bitIdents :: [String]
bitIdents = ["xor", "shiftL", "shiftR", "complement"]

{- | The configuration options
if 1 is provided, all mutants are selected for that kind, and 0 ensures that
no mutants are picked for that kind. Any fraction in between causes that
many mutants to be picked randomly from the available pool
-}
data Config = Config
    { -- \| Mutation operators on operator or function replacement
      muOp :: [FnOp]
    , -- \| Mutate pattern matches for functions?
      -- for example
      --
      -- > first [] = Nothing
      -- > first (x:_) = Just x
      --
      -- is mutated to
      --
      -- > first (x:_) = Just x
      -- > first [] = Nothing
      doMutatePatternMatches :: Rational
    , -- \| Mutates integer values by +1 or -1 or by replacing it with 0 or 1
      doMutateValues :: Rational
    , -- \| Mutates operators and functions, that is
      --
      -- > i + 1
      --
      -- becomes
      --
      -- > i - 1
      --
      -- > i * 1
      --
      -- > i / 1
      doMutateFunctions :: Rational
    , -- \| negate if conditions, that is
      --
      -- > if True then 1 else 0
      --
      -- becomes
      --
      -- > if True then 0 else 1
      doNegateIfElse :: Rational
    , -- \| negate guarded booleans in guarded definitions
      --
      -- > myFn x | x == 1 = True
      -- > myFn   | otherwise = False
      --
      -- becomes
      --
      -- > myFn x | not (x == 1) = True
      -- > myFn   | otherwise = False
      doNegateGuards :: Rational
    , -- \| Maximum number of mutants to generate.
      maxNumMutants :: Int
    }
    deriving (Show, Read)

-- | The default configuration
defaultConfig :: Config
defaultConfig =
    Config
        { muOp =
            [ FnOp{_type = FnIdent, _fns = predNums}
            , FnOp{_type = FnIdent, _fns = arithLists}
            , FnOp{_type = FnSymbol, _fns = comparators}
            , FnOp{_type = FnSymbol, _fns = binAriths}
            , FnOp{_type = FnSymbol, _fns = bitSymbols}
            , FnOp{_type = FnIdent, _fns = bitIdents}
            , FnOp{_type = FnSymbol, _fns = logicOps}
            , FnOp{_type = FnIdent, _fns = foldFns}
            ]
        , doMutatePatternMatches = 1.0
        , doMutateValues = 1.0
        , doMutateFunctions = 1.0
        , doNegateIfElse = 1.0
        , doNegateGuards = 1.0
        , maxNumMutants = 300
        }

-- | Enumeration of different variants of mutations
data MuVar
    = MutatePatternMatch
    | MutateValues
    | MutateFunctions
    | MutateNegateIfElse
    | MutateNegateGuards
    | MutateOther String
    deriving (Eq, Ord, Show, Read)

{- | getSample returns the fraction in config corresponding to the enum passed
in
-}
getSample :: MuVar -> Config -> Rational
getSample MutatePatternMatch c = doMutatePatternMatches c
getSample MutateValues c = doMutateValues c
getSample MutateFunctions c = doMutateFunctions c
getSample MutateNegateIfElse c = doNegateIfElse c
getSample MutateNegateGuards c = doNegateGuards c
getSample MutateOther{} _c = 1

{- | similarity between two mutation variants. For ease of use, MutateOther is
treated differently. For MutateOther, if the string is empty, then it is
matched against any other MutateOther.
-}
similar :: MuVar -> MuVar -> Bool
similar (MutateOther a) (MutateOther b) = null a || null b || a == b
similar x y = x == y

-- | Convert a 'MuVar' to its canonical user-facing name used by @--disable@\/@--enable@.
showMuVar :: MuVar -> String
showMuVar MutatePatternMatch              = "pattern-match"
showMuVar MutateValues                    = "literal-values"
showMuVar MutateFunctions                 = "functions"
showMuVar MutateNegateIfElse              = "negate-if-else"
showMuVar MutateNegateGuards              = "negate-guards"
showMuVar (MutateOther "remove-not")      = "remove-not"
showMuVar (MutateOther "remove-negation") = "remove-negation"
showMuVar (MutateOther s)                 = if null s then "other" else "other:" ++ s

-- | Parse a canonical mutator name (as produced by 'showMuVar') back to 'MuVar'.
-- Returns 'Nothing' for unrecognised strings.
-- Accepts trailing-@*@ wildcard patterns via 'matchesMuVarPat'.
-- Round-trips with 'showMuVar': @parseMuVar (showMuVar x) == Just x@.
parseMuVar :: String -> Maybe MuVar
parseMuVar "pattern-match"   = Just MutatePatternMatch
parseMuVar "literal-values"  = Just MutateValues
parseMuVar "functions"       = Just MutateFunctions
parseMuVar "negate-if-else"  = Just MutateNegateIfElse
parseMuVar "negate-guards"   = Just MutateNegateGuards
parseMuVar "remove-not"      = Just (MutateOther "remove-not")
parseMuVar "remove-negation" = Just (MutateOther "remove-negation")
parseMuVar "other"           = Just (MutateOther "")
parseMuVar s
  | Just rest <- stripPrefix "other:" s = Just (MutateOther rest)
parseMuVar _ = Nothing

-- | Match a user-supplied pattern (possibly with trailing @*@) against a 'MuVar' name.
matchesMuVarPat :: String -> MuVar -> Bool
matchesMuVarPat pat v =
  let name = showMuVar v
  in case reverse pat of
       ('*' : revPrefix) -> reverse revPrefix `isPrefixOf` name
       _                 -> pat == name
