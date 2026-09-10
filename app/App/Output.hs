{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Output, logging, and reporting functions.
module App.Output
  ( mutatorDescription
  , MutantDiff
  , prepareMutantDiffs
  , writeAgenticJsonLogger
  , writeAgenticJsonLoggerWithDiffs
  , writeHtmlLogger
  , writeHtmlLoggerWithDiffs
  , buildHtmlReport
  , buildHtmlReportWithDiffs
  , writeGithubLogger
  , writeGitlabLogger
  , writeUpdateBaseline
  , writeJsonLogger
  , printMutatorBreakdown
  , printMutantDetails
  , printMutantDetailsWithDiffs
  , unifiedDiff
  , groupConsec
  ) where

import Control.Monad (forM_, unless, when)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort)
import qualified Data.List as List
import Data.Maybe (fromMaybe)

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

-- | A mutation result paired with its rendered positional diff.
data MutantDiff = MutantDiff MutantSummary String

-- | Produce a compact unified diff between two source strings.
-- Shows only changed lines with up to 2 lines of context.
unifiedDiff :: String -> String -> String
unifiedDiff origSrc mutSrc
  | origSrc == mutSrc = ""
  | otherwise = concat (renderHunks oldCount newCount compared hunks)
  where
    oldLines = lines origSrc
    newLines = lines mutSrc
    oldCount = length oldLines
    newCount = length newLines
    compared = compareLines oldLines newLines
    hunks = contextHunks (max oldCount newCount) compared

-- | Pair each mutation result with one lazily rendered positional diff.
prepareMutantDiffs :: String -> [MutantSummary] -> [MutantDiff]
prepareMutantDiffs origSrc = map pairDiff
  where
    pairDiff summary =
      MutantDiff summary (unifiedDiff origSrc (_mutant (mutantOfSummary summary)))

mutantOfSummary :: MutantSummary -> Mutant
mutantOfSummary (MSumKilled m _)   = m
mutantOfSummary (MSumAlive m _)    = m
mutantOfSummary (MSumError m _ _)  = m
mutantOfSummary (MSumSkipped m _)  = m
mutantOfSummary (MSumOther m _)    = m

data DiffLine = DiffLine Int (Maybe String) (Maybe String) Bool

data DiffHunk = DiffHunk
  { hunkStart :: Int
  , hunkEnd :: Int
  }

contextWidth :: Int
contextWidth = 2

compareLines :: [String] -> [String] -> [DiffLine]
compareLines = go 1
  where
    go _ [] [] = []
    go number (oldLine : oldLines) [] =
      DiffLine number (Just oldLine) Nothing True
        : go (number + 1) oldLines []
    go number [] (newLine : newLines) =
      DiffLine number Nothing (Just newLine) True
        : go (number + 1) [] newLines
    go number (oldLine : oldLines) (newLine : newLines) =
      DiffLine number (Just oldLine) (Just newLine) (oldLine /= newLine)
        : go (number + 1) oldLines newLines

contextHunks :: Int -> [DiffLine] -> [DiffHunk]
contextHunks maxLine = reverse . List.foldl' addHunk []
  where
    addHunk hunks (DiffLine number _ _ True) =
      let start = max 1 (number - contextWidth)
          end = min maxLine (number + contextWidth)
      in case hunks of
           [] -> [DiffHunk start end]
           current : rest
             | start <= hunkEnd current + 1 ->
                 DiffHunk (hunkStart current) (max (hunkEnd current) end) : rest
             | otherwise -> DiffHunk start end : hunks
    addHunk hunks _ = hunks

renderHunks :: Int -> Int -> [DiffLine] -> [DiffHunk] -> [String]
renderHunks _ _ _ [] = []
renderHunks oldCount newCount compared (DiffHunk start end : hunks) =
  let atStart = dropBefore start compared
      (rows, remaining) = renderRows end atStart
  in renderHeader oldCount newCount start end
       : (rows ++ renderHunks oldCount newCount remaining hunks)

dropBefore :: Int -> [DiffLine] -> [DiffLine]
dropBefore _ [] = []
dropBefore start entries@(DiffLine number _ _ _ : rest)
  | number < start = dropBefore start rest
  | otherwise = entries

renderRows :: Int -> [DiffLine] -> ([String], [DiffLine])
renderRows end = go []
  where
    go acc [] = (reverse acc, [])
    go acc entries@(entry@(DiffLine number _ _ _) : rest)
      | number > end = (reverse acc, entries)
      | otherwise = go (reverse (renderLine entry) ++ acc) rest

renderLine :: DiffLine -> [String]
renderLine (DiffLine _ oldLine newLine changed)
  | changed = renderChanged '-' oldLine ++ renderChanged '+' newLine
  | otherwise = [" " ++ fromMaybe "" oldLine ++ "\n"]
  where
    renderChanged prefix = maybe [] (\line -> [prefix : line ++ "\n"])

renderHeader :: Int -> Int -> Int -> Int -> String
renderHeader oldCount newCount start end =
  "@@ -" ++ show oldStart ++ "," ++ show oldHunkCount
    ++ " +" ++ show newStart ++ "," ++ show newHunkCount
    ++ " @@\n"
  where
    oldHunkCount = linesInRange start end oldCount
    newHunkCount = linesInRange start end newCount
    oldStart = if oldHunkCount == 0 then 0 else start
    newStart = if newHunkCount == 0 then 0 else start

linesInRange :: Int -> Int -> Int -> Int
linesInRange start end lineCount
  | start > lineCount = 0
  | otherwise = min end lineCount - start + 1

-- | Group a sorted list of ints into runs of consecutive integers.
groupConsec :: [Int] -> [[Int]]
groupConsec [] = []
groupConsec (x:xs) = go [x] x xs
  where
    go cur _    []     = [reverse cur]
    go cur prev (y:ys)
      | y == prev + 1  = go (y : cur) y ys
      | otherwise      = reverse cur : go [y] y ys

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
          in object
             [ "description" .= desc
             , "fingerprint" .= fp
             , "severity" .= ("major" :: String)
             , "location" .= object
                 [ "path" .= file
                 , "lines" .= object [ "begin" .= ln ]
                 ]
             ]
    BL.writeFile path (BL.snoc (encode (map entry aliveMs)) 10)

-- | Write a per-mutant agentic JSON file for LLM consumption.
writeAgenticJsonLogger :: Opts -> FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> IO ()
writeAgenticJsonLogger opts file origSrc tsum msum =
  writeAgenticJsonLoggerWithDiffs opts file origSrc (prepareMutantDiffs origSrc tsum) msum

-- | Write agentic JSON using diffs prepared by the caller.
writeAgenticJsonLoggerWithDiffs :: Opts -> FilePath -> String -> [MutantDiff] -> MAnalysisSummary -> IO ()
writeAgenticJsonLoggerWithDiffs opts file origSrc diffs msum = case optLoggerAgenticJson opts of
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
          in  concatMap (\(i, l) -> "    " ++ show i ++ ": " ++ l ++ "\n") numbered
        entry (MutantDiff s diffText) =
          let m   = mutOf s
              ln  = spanStartLine (_mspan m)
              res = resultOf s
              desc = mutatorDescription (_mtype m)
              mid  = hash (_mutant m)
              ctx  = contextFor ln
          in object
              [ "id" .= mid
              , "type" .= showMuVar (_mtype m)
              , "file" .= file
              , "line" .= ln
              , "description" .= desc
              , "context_start_line" .= max 1 (ln - contextWindow)
              , "context" .= ctx
              , "diff" .= diffText
              , "result" .= res
              , "reminder" .= ("If result is alive, this mutation was not detected by any test. Consider adding a test that exercises this code path." :: String)
              ]
        entries = map entry diffs
        summaryJson = object
          [ "total" .= _maNumMutants
          , "killed" .= _maKilled
          , "alive" .= _maAlive
          , "skipped" .= _maSkipped
          , "errors" .= _maErrors
          , "msi" .= msiVal
          ]
        json = object
          [ "mutants" .= entries
          , "summary" .= summaryJson
          ]
    BL.writeFile path (BL.snoc (encode json) 10)

-- | Write a standalone HTML mutation report.
writeHtmlLogger :: Opts -> FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> IO ()
writeHtmlLogger opts file origSrc tsum msum =
  writeHtmlLoggerWithDiffs opts file origSrc (prepareMutantDiffs origSrc tsum) msum

-- | Write the HTML report using diffs prepared by the caller.
writeHtmlLoggerWithDiffs :: Opts -> FilePath -> String -> [MutantDiff] -> MAnalysisSummary -> IO ()
writeHtmlLoggerWithDiffs opts file origSrc diffs msum = case optLoggerHtml opts of
  Nothing   -> return ()
  Just path -> writeFile path (buildHtmlReportWithDiffs file origSrc diffs msum)

buildHtmlReport :: FilePath -> String -> [MutantSummary] -> MAnalysisSummary -> String
buildHtmlReport file origSrc tsum msum =
  buildHtmlReportWithDiffs file origSrc (prepareMutantDiffs origSrc tsum) msum

-- | Build the HTML report using diffs prepared by the caller.
buildHtmlReportWithDiffs :: FilePath -> String -> [MutantDiff] -> MAnalysisSummary -> String
buildHtmlReportWithDiffs file origSrc diffs msum =
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
      diffBlock diffText =
        if null diffText then "<em>no diff</em>"
        else "<pre class=\"diff\">" ++ esc diffText ++ "</pre>"
      entryHtml (MutantDiff s diffText) =
        let m  = mutOf s
            sc = statusClass s
            sl = statusLabel s
            mid = hash (_mutant m)
        in "<div class=\"mutant " ++ sc ++ "\">"
           ++ "<div class=\"mh\"><span class=\"st " ++ sc ++ "\">" ++ sl ++ "</span>"
           ++ " <span class=\"mv\">" ++ esc (showMuVar (_mtype m)) ++ "</span>"
           ++ " <span class=\"id\">ID:" ++ esc mid ++ "</span></div>"
           ++ "<div class=\"ctx\"><table>" ++ contextRows m ++ "</table></div>"
           ++ "<div class=\"df\">" ++ diffBlock diffText ++ "</div></div>\n"
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
     ++ concatMap entryHtml diffs
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
printMutantDetails opts origSrc sums =
  printMutantDetailsWithDiffs opts (prepareMutantDiffs origSrc sums)

-- | Print mutant details using diffs prepared by the caller.
printMutantDetailsWithDiffs :: Opts -> [MutantDiff] -> IO ()
printMutantDetailsWithDiffs opts diffs = do
  let filterStatuses (MutantDiff s _) = case optOutputStatuses opts of
                           Nothing -> True
                           Just chars -> case s of
                             MSumKilled  _ _   -> 'k' `elem` chars
                             MSumAlive   _ _   -> 'a' `elem` chars
                             MSumError{}       -> 'e' `elem` chars
                             MSumSkipped _ _   -> 's' `elem` chars
                             MSumOther   _ _   -> 'k' `elem` chars
      shouldShowQuiet (MutantDiff s _) = not (optQuiet opts) || case s of { MSumAlive _ _ -> True; _ -> False }
      toShow = filter (\s -> filterStatuses s && shouldShowQuiet s) diffs

  forM_ toShow $ \(MutantDiff s diffText) -> do
    let (status, Mutant{..}, logS, mErr) = case s of
                                     MSumKilled  mut l   -> ("KILLED",  mut, l, Nothing)
                                     MSumAlive   mut l   -> ("ALIVE",   mut, l, Nothing)
                                     MSumError   mut e l -> ("ERROR",   mut, l, Just e)
                                     MSumSkipped mut l   -> ("SKIPPED", mut, l, Nothing)
                                     MSumOther   mut l   -> ("OTHER",   mut, l, Nothing)
    putStrLn $ ">>> Mutant " ++ hash _mutant ++ " [" ++ status ++ "] " ++ showMuVar _mtype
    unless (optNoDiffs opts) $
      unless (null diffText) $ putStr diffText
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
