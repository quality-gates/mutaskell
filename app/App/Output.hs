{-# LANGUAGE RecordWildCards #-}
-- | Output, logging, and reporting functions.
module App.Output
  ( mutatorDescription
  , writeAgenticJsonLogger
  , writeHtmlLogger
  , buildHtmlReport
  , writeGithubLogger
  , writeGitlabLogger
  , writeUpdateBaseline
  , writeJsonLogger
  , printMutatorBreakdown
  , printMutantDetails
  , unifiedDiff
  , groupConsec
  ) where

import Control.Monad (forM_, unless, when)
import Data.List (intercalate, nub, sort)

import App.Opts (Opts(..))
import Test.Mutaskell.AnalysisSummary (MAnalysisSummary(..))
import Test.Mutaskell.Config (MuVar(..), showMuVar)
import Test.Mutaskell.Interpreter (MutantSummary(..))
import Test.Mutaskell.TestAdapter (Mutant(..))
import Test.Mutaskell.Tix (spanStartLine)
import Test.Mutaskell.Utils.Common (hash)

-- | Human-readable description of what a mutator changes.
mutatorDescription :: MuVar -> String
mutatorDescription MutatePatternMatch              = "Permute or remove a function pattern match"
mutatorDescription MutateValues                    = "Replace a literal value with a neighbouring value"
mutatorDescription MutateFunctions                 = "Replace an operator or function with a similar one"
mutatorDescription MutateNegateIfElse              = "Swap the then and else branches of an if expression"
mutatorDescription MutateNegateGuards              = "Wrap a guard condition in 'not'"
mutatorDescription (MutateOther "remove-not")      = "Remove 'not' from a negated sub-expression"
mutatorDescription (MutateOther "remove-negation") = "Remove 'negate' or prefix '-' from an expression"
mutatorDescription (MutateOther "case-alt-remove") = "Remove one alternative from a case expression"
mutatorDescription (MutateOther "case-default-remove") = "Remove the catch-all alternative from a case or guard"
mutatorDescription (MutateOther "remove-stmt")     = "Remove one statement from a do-block"
mutatorDescription (MutateOther "remove-let-binding") = "Remove one binding from a let or where clause"
mutatorDescription (MutateOther "remove-where-binding") = "Remove one binding from a where clause"
mutatorDescription (MutateOther "remove-self-assign") = "Remove a self-assignment (let x = x or x <- return x)"
mutatorDescription (MutateOther "negate-literal")  = "Replace a positive numeric literal with its negation"
mutatorDescription (MutateOther "string-literal")  = "Replace a string literal in a comparison with \"\""
mutatorDescription (MutateOther "bool-operand")    = "Replace a Boolean operand in && or || with True or False"
mutatorDescription (MutateOther "flip-maybe")      = "Flip Just x to Nothing or Nothing to Just undefined"
mutatorDescription (MutateOther "flip-either")     = "Flip Right x to Left x or Left x to Right x"
mutatorDescription (MutateOther "remove-forkIO")   = "Remove forkIO/async/withAsync concurrency wrapper"
mutatorDescription (MutateOther "bracket-degenerate") = "Replace bracket with acquire >>= action, removing cleanup"
mutatorDescription (MutateOther "error-guard")     = "Replace exception handler with a no-op"
mutatorDescription (MutateOther "replace-mutable-arg") = "Replace IORef/MVar/TVar argument with undefined"
mutatorDescription (MutateOther "zero-return")     = "Replace function body with zero value for declared return type"
mutatorDescription (MutateOther s)                 = "Apply mutator: " ++ s

-- | Produce a compact unified diff between two source strings.
-- Shows only changed lines with up to 2 lines of context.
unifiedDiff :: String -> String -> String
unifiedDiff origSrc mutSrc
  | origSrc == mutSrc = ""
  | otherwise = concatMap renderHunk hunks
  where
    oLines   = lines origSrc
    mLines   = lines mutSrc
    maxLen   = max (length oLines) (length mLines)
    ctx      = 2
    getL i ls = if i <= length ls then ls !! (i - 1) else ""
    diffIdxs = [i | i <- [1..maxLen], getL i oLines /= getL i mLines]
    ctxSet   = nub $ sort $ concatMap (\i -> [max 1 (i-ctx)..min maxLen (i+ctx)]) diffIdxs
    hunks    = groupConsec ctxSet
    renderHunk hunk@(lo:_) =
      let hi  = hunk !! (length hunk - 1)
          hdr = "@@ -" ++ show lo ++ "," ++ show (hi - lo + 1) ++ " @@\n"
          rows = concat (concatMap renderRow hunk)
      in hdr ++ rows
    renderHunk [] = ""
    renderRow i
      | i `elem` diffIdxs =
          let oL = getL i oLines; mL = getL i mLines
          in  ["-" ++ oL ++ "\n" | not (null oL) || i <= length oLines]
           ++ ["+" ++ mL ++ "\n" | not (null mL) || i <= length mLines]
      | otherwise = [" " ++ getL i oLines ++ "\n"]

-- | Group a sorted list of ints into runs of consecutive integers.
groupConsec :: [Int] -> [[Int]]
groupConsec [] = []
groupConsec (x:xs) = go [x] x xs
  where
    go cur _    []     = [cur]
    go cur prev (y:ys)
      | y == prev + 1  = go (cur ++ [y]) y ys
      | otherwise      = cur : go [y] y ys

-- | Write surviving mutant IDs to the update-baseline file.
writeUpdateBaseline :: Opts -> [MutantSummary] -> IO ()
writeUpdateBaseline opts tsum = case optUpdateBaseline opts of
  Nothing   -> return ()
  Just path -> do
    let aliveIds = [hash (_mutant m) | MSumAlive m _ <- tsum]
    writeFile path (unlines aliveIds)

-- | Write a compact JSON summary to the logger-json file.
writeJsonLogger :: Opts -> MAnalysisSummary -> IO ()
writeJsonLogger opts msum = case optLoggerJson opts of
  Nothing   -> return ()
  Just path -> do
    let MAnalysisSummary{..} = msum
        noerrors = _maNumMutants - _maErrors
        msiVal :: Double
        msiVal = if noerrors > 0
                 then fromIntegral _maKilled / fromIntegral noerrors
                 else 0.0
        covNoerrors = if _maCoveredNumMutants > 0
                      then _maCoveredNumMutants - _maErrors
                      else noerrors
        covMsiVal :: Double
        covMsiVal = if covNoerrors > 0
                    then fromIntegral _maKilled / fromIntegral covNoerrors
                    else 0.0
        covMsiField = if _maCoveredNumMutants > 0
                      then show covMsiVal
                      else "null"
        json = unlines
          [ "{"
          , "  \"total\": " ++ show _maNumMutants ++ ","
          , "  \"killed\": " ++ show _maKilled ++ ","
          , "  \"alive\": " ++ show _maAlive ++ ","
          , "  \"skipped\": " ++ show _maSkipped ++ ","
          , "  \"errors\": " ++ show _maErrors ++ ","
          , "  \"msi\": " ++ show msiVal ++ ","
          , "  \"covered_code_msi\": " ++ covMsiField
          , "}"
          ]
    writeFile path json

-- | Write GitHub Actions annotation lines for escaped mutants.
writeGithubLogger :: Opts -> FilePath -> [MutantSummary] -> IO ()
writeGithubLogger opts file tsum = case optLoggerGithub opts of
  Nothing   -> return ()
  Just path -> do
    let aliveSums = [s | s@(MSumAlive _ _) <- tsum]
        line s = case s of
          MSumAlive m _ ->
            let ln  = spanStartLine (_mspan m)
                msg = "Mutant survived: " ++ showMuVar (_mtype m) ++ " (ID: " ++ hash (_mutant m) ++ ")"
            in "::warning file=" ++ file ++ ",line=" ++ show ln ++ ",col=1::" ++ msg
          _ -> ""
        annotations = map line aliveSums
    writeFile path (unlines annotations)

-- | Write a GitLab Code Quality JSON artifact for escaped mutants.
writeGitlabLogger :: Opts -> FilePath -> [MutantSummary] -> IO ()
writeGitlabLogger opts file tsum = case optLoggerGitlab opts of
  Nothing   -> return ()
  Just path -> do
    let aliveMs = [m | MSumAlive m _ <- tsum]
        entry m =
          let ln   = spanStartLine (_mspan m)
              fp   = hash (_mutant m)
              desc = "Mutant survived: " ++ showMuVar (_mtype m)
          in intercalate "\n"
             [ "  {"
             , "    \"description\": " ++ show desc ++ ","
             , "    \"fingerprint\": " ++ show fp ++ ","
             , "    \"severity\": \"major\","
             , "    \"location\": {"
             , "      \"path\": " ++ show file ++ ","
             , "      \"lines\": { \"begin\": " ++ show ln ++ " }"
             , "    }"
             , "  }"
             ]
        entries = map entry aliveMs
        body    = intercalate ",\n" entries
    writeFile path $ "[\n" ++ body ++ (if null entries then "" else "\n") ++ "]\n"

-- | Write a per-mutant agentic JSON file for LLM consumption.
writeAgenticJsonLogger :: Opts -> FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> IO ()
writeAgenticJsonLogger opts file origSrc tsum msum = case optLoggerAgenticJson opts of
  Nothing   -> return ()
  Just path -> do
    let MAnalysisSummary{..} = msum
        noerrors = _maNumMutants - _maErrors
        msiVal :: Double
        msiVal = if noerrors > 0
                 then fromIntegral _maKilled / fromIntegral noerrors
                 else 0.0
        resultOf (MSumKilled  _ _)   = "killed"  :: String
        resultOf (MSumAlive   _ _)   = "alive"
        resultOf MSumError{}         = "error"
        resultOf (MSumSkipped _ _)   = "skipped"
        resultOf (MSumOther   _ _)   = "other"
        mutOf (MSumKilled  m _)   = m
        mutOf (MSumAlive   m _)   = m
        mutOf (MSumError   m _ _) = m
        mutOf (MSumSkipped m _)   = m
        mutOf (MSumOther   m _)   = m
        oLines = lines origSrc
        contextWindow = 3
        contextFor ln =
          let start = max 1 (ln - contextWindow)
              end   = min (length oLines) (ln + contextWindow)
              numbered = zip [start..] (drop (start - 1) (take end oLines))
          in  concatMap (\(i, l) -> "    " ++ show i ++ ": " ++ l ++ "\\n") numbered
        entry s =
          let m   = mutOf s
              ln  = spanStartLine (_mspan m)
              res = resultOf s
              d   = show $ unifiedDiff origSrc (_mutant m)
              desc = mutatorDescription (_mtype m)
              mid  = hash (_mutant m)
              ctx  = contextFor ln
          in  intercalate "\n"
              [ "  {"
              , "    \"id\": " ++ show mid ++ ","
              , "    \"type\": " ++ show (showMuVar (_mtype m)) ++ ","
              , "    \"file\": " ++ show file ++ ","
              , "    \"line\": " ++ show ln ++ ","
              , "    \"description\": " ++ show desc ++ ","
              , "    \"context_start_line\": " ++ show (max 1 (ln - contextWindow)) ++ ","
              , "    \"context\": " ++ show ctx ++ ","
              , "    \"diff\": " ++ d ++ ","
              , "    \"result\": " ++ show res ++ ","
              , "    \"reminder\": \"If result is alive, this mutation was not detected by any test. Consider adding a test that exercises this code path.\""
              , "  }"
              ]
        entries = map entry tsum
        mutantsBody = intercalate ",\n" entries
        summaryJson = intercalate "\n"
          [ "  \"summary\": {"
          , "    \"total\": " ++ show _maNumMutants ++ ","
          , "    \"killed\": " ++ show _maKilled ++ ","
          , "    \"alive\": " ++ show _maAlive ++ ","
          , "    \"skipped\": " ++ show _maSkipped ++ ","
          , "    \"errors\": " ++ show _maErrors ++ ","
          , "    \"msi\": " ++ show msiVal
          , "  }"
          ]
        json = "{\n  \"mutants\": [\n" ++ mutantsBody ++
               (if null entries then "" else "\n") ++
               "  ],\n" ++ summaryJson ++ "\n}\n"
    writeFile path json

-- | Write a standalone HTML mutation report.
writeHtmlLogger :: Opts -> FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> IO ()
writeHtmlLogger opts file origSrc tsum msum = case optLoggerHtml opts of
  Nothing   -> return ()
  Just path -> writeFile path (buildHtmlReport file origSrc tsum msum)

buildHtmlReport :: FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> String
buildHtmlReport file origSrc tsum msum =
  let MAnalysisSummary{..} = msum
      noerrors = _maNumMutants - _maErrors
      msiPct :: Int
      msiPct = if noerrors > 0 then _maKilled * 100 `div` noerrors else 0
      esc = concatMap escChar
      escChar '<' = "&lt;"; escChar '>' = "&gt;"
      escChar '&' = "&amp;"; escChar '"' = "&quot;"
      escChar c   = [c]
      statusClass (MSumKilled  _ _)   = "killed"  :: String
      statusClass (MSumAlive   _ _)   = "alive"
      statusClass MSumError{}         = "error"
      statusClass (MSumSkipped _ _)   = "skipped"
      statusClass (MSumOther   _ _)   = "other"
      statusLabel (MSumKilled  _ _)   = "KILLED"  :: String
      statusLabel (MSumAlive   _ _)   = "ALIVE"
      statusLabel MSumError{}         = "ERROR"
      statusLabel (MSumSkipped _ _)   = "SKIPPED"
      statusLabel (MSumOther   _ _)   = "OTHER"
      mutOf (MSumKilled  m _)   = m; mutOf (MSumAlive   m _)   = m
      mutOf (MSumError   m _ _) = m; mutOf (MSumSkipped m _)   = m
      mutOf (MSumOther   m _)   = m
      oLines = lines origSrc
      ctxWin = 3 :: Int
      contextRows m =
        let sl    = spanStartLine (_mspan m)
            start = max 1 (sl - ctxWin)
            end   = min (length oLines) (sl + ctxWin)
            nums  = [start..end]
            lns   = drop (start - 1) (take end oLines)
        in concatMap (\(i,l) ->
              "<tr" ++ (if i == sl then " class=\"hl\"" else "") ++ ">"
              ++ "<td class=\"ln\">" ++ show i ++ "</td>"
              ++ "<td><code>" ++ esc l ++ "</code></td></tr>")
           (zip nums lns)
      diffBlock m =
        let d = unifiedDiff origSrc (_mutant m)
        in if null d then "<em>no diff</em>"
           else "<pre class=\"diff\">" ++ esc d ++ "</pre>"
      entryHtml s =
        let m  = mutOf s
            sc = statusClass s
            sl = statusLabel s
            mid = hash (_mutant m)
        in "<div class=\"mutant " ++ sc ++ "\">"
           ++ "<div class=\"mh\"><span class=\"st " ++ sc ++ "\">" ++ sl ++ "</span>"
           ++ " <span class=\"mv\">" ++ esc (showMuVar (_mtype m)) ++ "</span>"
           ++ " <span class=\"id\">ID:" ++ esc mid ++ "</span></div>"
           ++ "<div class=\"ctx\"><table>" ++ contextRows m ++ "</table></div>"
           ++ "<div class=\"df\">" ++ diffBlock m ++ "</div></div>\n"
      css = concat
        [ "body{font-family:monospace;margin:20px}"
        , ".sum{background:#f5f5f5;padding:12px;border-radius:4px;margin-bottom:16px}"
        , ".mutant{border:1px solid #ddd;margin-bottom:12px;border-radius:4px;overflow:hidden}"
        , ".mh{padding:6px 10px;display:flex;gap:12px;align-items:center}"
        , ".killed .mh{background:#d4edda}.alive .mh{background:#f8d7da}"
        , ".error .mh{background:#fff3cd}.skipped .mh{background:#e2e3e5}"
        , ".st{padding:2px 6px;border-radius:3px;font-size:.85em;font-weight:bold}"
        , ".st.killed{background:#28a745;color:#fff}.st.alive{background:#dc3545;color:#fff}"
        , ".st.error{background:#ffc107;color:#000}.st.skipped{background:#6c757d;color:#fff}"
        , ".id{color:#666;font-size:.8em}.mv{font-weight:bold}"
        , ".ctx table{border-collapse:collapse;width:100%;padding:4px 8px}"
        , ".ctx td{padding:1px 6px;font-size:.85em}.ln{color:#999;text-align:right;border-right:1px solid #eee}"
        , ".hl td{background:#fffde7}"
        , ".df pre{margin:0;padding:6px 10px;background:#f8f8f8;font-size:.85em;overflow-x:auto}"
        ]
  in "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">"
     ++ "<title>MuCheck: " ++ esc file ++ "</title>"
     ++ "<style>" ++ css ++ "</style></head><body>"
     ++ "<h1>Mutation Report: " ++ esc file ++ "</h1>"
     ++ "<div class=\"sum\"><strong>MSI:</strong> " ++ show msiPct ++ "% &nbsp; "
     ++ "<strong>Total:</strong> " ++ show _maNumMutants ++ " &nbsp; "
     ++ "<strong>Killed:</strong> " ++ show _maKilled ++ " &nbsp; "
     ++ "<strong>Alive:</strong> " ++ show _maAlive ++ " &nbsp; "
     ++ "<strong>Skipped:</strong> " ++ show _maSkipped ++ " &nbsp; "
     ++ "<strong>Errors:</strong> " ++ show _maErrors ++ "</div>"
     ++ concatMap entryHtml tsum
     ++ "</body></html>\n"

printMutatorBreakdown :: Opts -> [MutantSummary] -> IO ()
printMutatorBreakdown _ [] = return ()
printMutatorBreakdown _opts sums = do
  let mutOf (MSumError   m _ _) = m
      mutOf (MSumSkipped m _)   = m
      mutOf (MSumAlive   m _)   = m
      mutOf (MSumKilled  m _)   = m
      mutOf (MSumOther   m _)   = m
      isKilled  (MSumKilled  _ _)   = True; isKilled  _ = False
      isAlive   (MSumAlive   _ _)   = True; isAlive   _ = False
      isErr     MSumError{}          = True; isErr     _ = False
      isSkipped (MSumSkipped _ _)   = True; isSkipped _ = False
      mutype    = showMuVar . _mtype . mutOf
      types     = sort . nub $ map mutype sums
      row t     = let ts = filter ((== t) . mutype) sums
                      k  = length $ filter isKilled  ts
                      a  = length $ filter isAlive   ts
                      e  = length $ filter isErr     ts
                      s  = length $ filter isSkipped ts
                  in (t, k, a, e, s)
      rows      = map row types
      colW      = max 8 $ maximum $ map (\(t,_,_,_,_) -> length t) rows
      pad s'    = s' ++ replicate (colW - length s' + 2) ' '
      sep       = replicate (colW + 42) '-'
      fmtN n    = replicate (max 0 (6 - length (show n))) ' ' ++ show n
  putStrLn ""
  putStrLn $ "  " ++ pad "Mutator" ++ "  Killed   Alive  Errors  Skipped"
  putStrLn sep
  mapM_ (\(t,k,a,e,s) -> putStrLn $ "  " ++ pad t ++ "  " ++ fmtN k ++ "  " ++ fmtN a ++ "  " ++ fmtN e ++ "  " ++ fmtN s) rows
  putStrLn sep

printMutantDetails :: Opts -> String -> [MutantSummary] -> IO ()
printMutantDetails opts origSrc sums = do
  let filterStatuses s = case optOutputStatuses opts of
                           Nothing -> True
                           Just chars -> case s of
                             MSumKilled  _ _   -> 'k' `elem` chars
                             MSumAlive   _ _   -> 'a' `elem` chars
                             MSumError{}       -> 'e' `elem` chars
                             MSumSkipped _ _   -> 's' `elem` chars
                             MSumOther   _ _   -> 'k' `elem` chars
      shouldShowQuiet s = not (optQuiet opts) || case s of { MSumAlive _ _ -> True; _ -> False }
      toShow = filter (\s -> filterStatuses s && shouldShowQuiet s) sums

  forM_ toShow $ \s -> do
    let (status, Mutant{..}, logS, mErr) = case s of
                                     MSumKilled  mut l   -> ("KILLED",  mut, l, Nothing)
                                     MSumAlive   mut l   -> ("ALIVE",   mut, l, Nothing)
                                     MSumError   mut e l -> ("ERROR",   mut, l, Just e)
                                     MSumSkipped mut l   -> ("SKIPPED", mut, l, Nothing)
                                     MSumOther   mut l   -> ("OTHER",   mut, l, Nothing)
    putStrLn $ ">>> Mutant " ++ hash _mutant ++ " [" ++ status ++ "] " ++ showMuVar _mtype
    unless (optNoDiffs opts) $
      let d = unifiedDiff origSrc _mutant
      in unless (null d) $ putStr d
    when (optVerbose opts) $ do
      putStrLn "--- Full source ---"
      putStrLn _mutant
      putStrLn "-------------------"
      mapM_ print logS
    when (optDebug opts) $
      case mErr of
        Just e  -> putStrLn $ "Error: " ++ e
        Nothing -> return ()
    putStrLn ""
