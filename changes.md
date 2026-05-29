# Changelog

## [0.6.2]
  * Fixed: licence reverted to GPL-2.0-or-later with full GPLv2 text; MIT relicensing was not permissible without consent from original copyright holders

## [0.6.1]
  * Fixed: removed committed internal tool files (`.claude/scheduled_tasks.lock`, `.serena/project.yml`, `todo.md`, `old.md`); added `.claude/` and `.serena/` to `.gitignore`
  * Fixed: stale `MuCheck.cabal` references in `haskell-ci.yml` comment header
  * Fixed: schema `$id`, `title`, and `description` updated to reference `mutaskell`
  * Fixed: CLAUDE.md corrected all `master` → `main` branch references
  * Fixed: all CI workflow `push`/`pull_request` branch filters updated from `master` to `main` so HLint, OSV-scanner, and mutation jobs trigger correctly

## [0.6.0]
  * Changed: package renamed from `MuCheck` to `mutaskell`; all library modules moved from `Test.MuCheck.*` to `Test.Mutaskell.*`; binary renamed from `mucheck` to `mutaskell`

## [0.5.8]
  * Added: `setups/` directory with ready-to-use GitHub Actions workflow, GitLab CI job, and three `.mucheck.yaml` templates (conservative, strict, diff-only)
  * Changed: README rewritten with plain-language explanation of why mutation testing matters, concrete examples of AI-generated test patterns that escape mutation, covered-MSI guidance, and a get-started section
  * Fixed: CLAUDE.md shipping workflow updated to prevent recurring version revert: version bump now required in the PR itself, and agents must branch from `origin/master` before doing any work

## [0.5.7]
  * Added: `&&`/`||` logical operator swap and `foldl`/`foldr` fold-direction swap to default function substitution groups
  * Added: 8 new dedicated mutation operators — `list-literal` (empty or shrink explicit list literals), `bind-to-sequence` (wildcard monadic binds), `pattern-constructor` (flip `Just`/`Nothing`, `Left`/`Right`, `True`/`False` in patterns), `append-strip` (drop one side of `++`), `flip-args` (swap arguments of known binary functions such as `compare`, `div`, `elem`), `seq-strip` (remove `seq x y` → `y`), `tuple-swap` (swap pair components), `ordering-literal` (flip `GT`↔`LT`, replace `EQ`)

## [0.5.6]
  * Changed: CI mutation job now caches the GHC 9.12.1 toolchain (`/usr/local/.ghcup`) and the Hackage package index (`~/.cabal/packages`) between runs; previously GHC was downloaded (286 MB) and installed from scratch on every run, costing ~1m45s; the cabal store was already cached

## [0.5.5]
  * Changed: README badges consolidated from five (Mutation Analysis, HLint, OSV-Scanner, Docs, License) to three (CI, Docs, License); HLint and OSV-Scanner badges removed
  * Added: "Mutation Types: Before & After" section in README with a concrete Haskell before/after example for each of the 23 mutation types
  * Changed: Haddock pages deployment now passes `--haddock-hyperlinked-source` so each identifier in the hosted docs links to a syntax-highlighted source view

## [0.5.4]
  * Fixed: `selectRemoveStmtOps` no longer applies to list comprehensions (`HsDo ListComp`); the previous `isValidDo` check incorrectly allowed removing the mandatory result `LastStmt` from a comprehension, leaving a body-less comprehension that triggered a GHC 9.12.1 `pprComp` panic; `isDo` is now restricted to `DoExpr`/`MDoExpr` only
  * Fixed: `--noop` pre-flight failure now prints the actual interpreter error to stderr so users can diagnose test format problems (e.g. wrong return type, missing imports) instead of receiving only the generic "test suite does not pass" message
  * Fixed: `--help` footer now uses `footerDoc`/`vsep` so mutator names and exit codes render as structured lines instead of a single reflowed paragraph

## [0.5.3]
  * Fixed: replaced the line-based worker IPC protocol (`workerSerialize`/`workerDeserialize`) with a self-contained JSON object; a single extra newline inside a mutant diff or test output could corrupt the line-based deserialiser; JSON handles embedded newlines safely; a `version` field is included for future schema evolution; verified correct results with `--workers 2` against `Examples/AssertCheckTest.hs`
  * Changed: migrated AST backend from `haskell-src-exts` to the GHC API (`ghc` + `ghc-exactprint >= 1.12`); the parser now uses GHC 9.12's actual parser so all language extensions (`LambdaCase`, `TypeFamilies`, `GADTs`, `LinearTypes`, etc.) are supported; previously any source using an unsupported extension was silently parsed as empty and produced zero mutants
  * Changed: `getASTFromStr` now returns `IO (Either String Module_)` instead of `Either String Module`; callers updated throughout the library and CLI; the `ghc --print-libdir` call is made once per parse, not once per run
  * Changed: mutant serialisation changed from `haskell-src-exts`'s `prettyPrint` to `ghc-exactprint`'s `exactPrint`; unchanged source regions are preserved exactly; several mutators (`negate-literal`, `zero-return`, `pattern-match`, `remove-where-binding`) were updated to emit correct `EpAnn` delta annotations so `exactPrint` produces compilable output
  * Changed: mutations are now generated from non-test declarations only but applied to the full module AST, so `exactPrint` can render every declaration at its original source position without EpAnn corruption
  * Fixed: `remove-self-assign` mutator now correctly recognises `let x = x in …` bindings that GHC parses as `FunBind` rather than `PatBind`
  * Removed: `haskell-src-exts` dependency; `MuOp`, `Mutation`, test helpers, and the `Here` quasi-quoter now use GHC's type aliases (`Module_`, `Expr_`, `Decl_`, etc.)
  * Changed: updated `tested-with` to `GHC ==9.12.1`

## [0.5.2]
  * Changed: replaced the hand-rolled `parseYamlKV` config loader with the `yaml` library; config files now correctly handle quoted strings, inline lists (`[a, b]`), block lists, and multi-line values that the old split-on-`:` parser rejected; unknown config keys are still rejected with a clear error listing valid keys
  * Added: `parseYamlConfigStr` exported from `App.Opts` for testing YAML config parsing from inline strings; three new tests in `CLISpec` cover inline lists, block lists, and unknown-key rejection

## [0.5.1]
  * Changed: replaced the hand-rolled `parseOpts`/`parseOptsFrom` CLI parser with `optparse-applicative`; the binary now generates `--help` output automatically and reports flag errors in the standard format; shell completion scripts are available via `--bash-completion-script` / `--zsh-completion-script`
  * Changed: the HPC coverage flag is now `--tix FILE` (was `-tix FILE`); update any scripts or CI configs that used the old single-dash form
  * Changed: `main` now uses `execParser` directly; the two-pass arg scanning is replaced with a lightweight `extractConfigArg` pre-scan for `--config`; the hand-rolled `help` function is removed in favour of optparse-applicative auto-generated help

## [0.5.0]
  * Changed: `tested-with` in `MuCheck.cabal` narrowed to `GHC ==9.8.2`; the broader matrix in `haskell-ci.yml` is disabled (`if: false`) because it exceeds the five-minute CI budget; broader compatibility is aspirational
  * Removed: `GenerationMode` type (`FirstOrderOnly`/`FirstAndHigherOrder`) and `genMode` field from `Config`; the field was never consulted by `programMutantsWith` (which always used `mutatesN` with order 1); removes dead code and simplifies the `Config` record
  * Added: `showMuVar`, `parseMuVar`, and `matchesMuVarPat` to `Test.MuCheck.Config`; library consumers can now convert `MuVar` values to/from strings and use the same pattern-matching logic as `--disable`/`--enable`; `parseMuVar` round-trips with `showMuVar`; `ConfigSpec` tests cover all constructors
  * Fixed: filter-before-sample ordering in the mutant pipeline; all deterministic filters (`--disable`/`--enable`, annotations, baseline, blacklist, diff-lines, ignore-lines, `--run-mutant-id`) are now applied before sampling; previously filters ran on the already-sampled set so quota was spent on candidates that were subsequently discarded; `--max-mutants` is now also passed to `sampler` directly
  * Changed: decomposed `app/Main.hs` into focused sub-modules: `App.Filter` (filter stages), `App.Output` (loggers and terminal output), `App.Worker` (subprocess parallelism and IPC), `App.Exit` (exit-code policy); `Main.hs` itself now contains only `main`, `runOpts`, `noopCheck`, `resolveTimeout`, `dryRun`, and `help`
  * Changed: removed the redundant once-based applicability probe from `relevantOps` in `Test.MuCheck.Utils.Syb`; the probe traversed the AST O(n_ops) extra times to check if each operator could fire, but `mutate` already handles non-applicable ops by returning an empty list; reduces the per-run traversal count by ~n_ops

## [0.4.23]
  * Fixed: `evalTest` now discovers the `.ghc.environment.*` file in the current directory and passes it to `unsafeRunInterpreterWithArgs` via `-package-env`; on GHC 9.8+ the GHC API no longer reads this file automatically, causing hint to report `WontCompile` for every mutant that imported a project-local package (the entire integration test suite was producing 0 kills); this restores correct behaviour across all supported GHC versions
  * Fixed: `evalMutant` now writes each mutant to a path matching its module name (e.g. `<tmpdir>/<hash>/Examples/AssertCheckTest.hs` for `module Examples.AssertCheckTest`) so that GHC can load it correctly via hint regardless of whether the version enforces file-path/module-name correspondence; previously all mutants were `WontCompile`-skipped on GHC 9.8.2 in CI
  * Fixed: `allTests` now combines both `{-# ANN #-}` annotation-based and naming-convention-based (`prop_*`, `test_*`, `spec_*`) test discovery rather than treating conventions as a fallback; a module that mixes both styles no longer silently drops convention-named tests from evaluation
  * Fixed: `getMix` in `Test.MuCheck.Tix` now returns `IO (Either String Mix)` instead of calling `error`; a missing `.mix` file prints `"Coverage error: cannot find <module> in .hpc — is the test suite built with -fhpc?"` to stderr and exits with code 2
  * Fixed: `--worker-output` is confirmed absent from the user-facing help text (`mucheck -h`); it remains parseable for internal subprocess IPC use only
  * Fixed: both `--logger-json` and `--logger-agentic-json` outputs confirmed to use a consistent 0–1 float scale for `msi`; added a README section documenting the `--logger-json` output format with a JSON example
  * Changed: `Examples/AssertCheckTest.hs` updated to demonstrate naming-convention auto-discovery (`test_*` and `prop_*` prefixes) alongside the legacy `{-# ANN #-}` annotation; `Examples/Main.hs` updated accordingly
  * Fixed: `test/Spec.hs` cleaned up — removed dead manual `main`/`spec` wiring that conflicted with the active `hspec-discover` pragma; added a comment explaining that new `*Spec.hs` files are picked up automatically
  * Added: `test/Test/MuCheck/IntegrationSpec.hs` — end-to-end test that calls the `mucheck` library function on `Examples/AssertCheckTest.hs` and asserts kills > 0 and count consistency; run selectively with `--test-option=--match --test-option="/integration/"`
  * Added: extended `Test.MuCheck.CLISpec` with 18 tests covering flag round-trips, bad-argument error messages, and config-file override behaviour
  * Fixed: reached 100% Haddock coverage across all modules; addressed missing documentation in `AssertCheck`, `AssertCheckAdapter`, and `Print`; fixed stale `testSummaryFn` and `MuOP` references in docstrings

## [0.4.22]
  * Fixed: passing both `--enable` and `--disable` together now exits with code 2 and a clear error message instead of silently letting `--enable` win
  * Changed: extracted `Opts`, `parseOptsFrom`, and related config functions into a new `App.Opts` module so the test suite can exercise CLI parsing directly
  * Added: six unit tests in `Test.MuCheck.CLISpec` covering the `--enable`/`--disable` conflict, unknown flags, and missing file argument

## [0.4.21]
  * Fixed: add `workers` to `schema/mucheck-config-schema.json`; editors with YAML autocomplete now suggest it and schema-validating tools no longer reject it

## [0.4.20]
  * Fixed: `getASTFromStr` now returns `Either String Module_` instead of crashing on parse failure
  * Fixed: `genMutants`, `getAllTests`, and `mucheck` now propagate parse errors gracefully to the terminal and exit with code 2

## [0.4.19]
  * Add `--workers N` flag: evaluate mutants concurrently using N subprocess workers; hint is not thread-safe so each worker is a fresh process; output is synchronized through the parent to prevent interleaved lines

## [0.4.18]
  * Fixed: HPC module name lookup to correctly handle package/target prefixes
  * Fixed: HPC .mix file loading robustness for flattened .hpc directories (common in CI)
  * Refactored: `removeRedundantSpans` for better robustness
  * Improved: MSI accuracy by correctly identifying uncovered code (example MSI improved from 59% to 76%)
  * Add `exclude_dirs` config key: list of path prefixes; skip mutation if the target file path starts with any listed prefix
  * Add `genMutantsWithExtra` API in `Test.MuCheck.Mutation`: accepts additional custom `(MuVar, Module_ -> [MuOp])` selector functions for third-party mutators without forking
  * Auto-discover test functions by naming convention (`prop_*`, `test_*`, `spec_*`) when no `{-# ANN ... "Test" #-}` annotations are present
  * Improve error display: `showE` now produces clean single-line summaries; `WontCompile` shows only the first GHC error message
  * Add covered-MSI >= 50% quality gate to CI mutation workflow using HPC coverage data; copy mix files from dist-newstyle to .hpc/ so getMix can resolve them

## [0.4.17]
  * Add `zero-return` mutator: replace each function match body with the zero value for its declared return type; applies to functions with a type signature in the same module (`Bool` → `False`, `Int`/`Integer` → `0`, `Double`/`Float` → `0.0`, `String` → `""`, `[a]` → `[]`, `Maybe a` → `Nothing`, `IO a` → `return undefined`)
  * Add 8 tests for `removeRedundantSpans` and `removeUncovered` (coverage-filtering code path)
  * Add MuVar coverage test for `zero-return`

## [0.4.16]
  * Add `ignore_source_lines` config key: list of substrings; suppress mutations on source lines containing any substring
  * Add `skip_without_test` config key: when true, skip source modules with no test annotations and exit cleanly
  * Fix output ordering race: after evaluation, read the final progress counts in the main thread and print them before the summary; eliminates interleaving between stderr progress and stdout output
  * Publish JSON Schema for the config file at `schema/mucheck-config-schema.json` with `additionalProperties: false` for editor autocomplete support

## [0.4.15]
  * Add `silent_mode` config key: print only the final summary line; suppress all per-mutant output and the mutator breakdown table
  * Add `max_mutants` config key: cap the total number of sampled mutants before evaluation
  * Add `json_output` config key: persistent path for JSON run summary, equivalent to `--logger-json` but set via config
  * Add `html_output` config key: persistent path for HTML report, equivalent to `--logger-html` but set via config
  * Reject unknown keys in the config file with a clear error message listing all known keys
  * Add a complete `.mucheck.yaml` example to the README documenting all supported config keys

## [0.4.14]
  * Add `--logger-html <file>` flag: write a standalone HTML mutation report with per-mutant diffs, source context, and colour-coded status badges
  * Add `--test-args <arg>` flag: pass additional arguments to the test runner on every invocation (repeatable); forwarded via `withArgs`
  * Add `--coverage` flag: auto-discover a `.tix` coverage file in the current directory without requiring `-tix FILE`
  * Add YAML config file support: auto-load `.mucheck.yaml` from the project root; supported keys: `min_msi`, `min_covered_msi`, `timeout`, `quiet`, `disable_mutators`, `enable_mutators`; CLI flags override config values
  * Add `--config <file>` flag: specify an alternate config file path instead of auto-loading `.mucheck.yaml`
  * Print a live progress line to stderr during evaluation showing kill/alive/error/skip counts; suppressed in `--quiet` mode
  * Add `evaluateMutants` optional per-mutant callback and `[String]` extra-args parameters to the library API
  * Complete the help text: document all flags including `--quiet`, `--verbose`, `--debug`, `--no-diffs`, `--timeout`, `--min-covered-msi`, and `--output-statuses`

## [0.4.13]
  * Add `--logger-agentic-json <file>` flag: write per-mutant JSON with stable IDs, descriptions, context lines, and MSI summary for LLM consumption
  * Add `--git-diff-lines` flag: restrict mutations to lines changed relative to `--git-diff-base` (requires `--git-diff-base`)
  * Add `--keep-mutants <dir>` flag: write mutant files to a named directory and preserve them after evaluation
  * Write mutant files to the system temp directory by default (instead of `.mutants/` in the project); clean up after each evaluation
  * Cache the parsed AST in `genMutantsWith` to avoid double-parsing the source file
  * Add 22 MuVar coverage tests ensuring each mutator produces at least one mutant on a minimal canonical input
  * Enable GitHub Pages for Haddock documentation deployment

## [0.4.12]
  * Add `--logger-github <file>` flag: write GitHub Actions `::warning` annotations for escaped mutants
  * Add `--logger-gitlab <file>` flag: write a GitLab Code Quality JSON artifact for escaped mutants
  * Track non-compilable mutants as `Skipped` (distinct from interpreter `Errors`) in the summary and per-mutator breakdown table
  * Add `MSumSkipped` constructor to `MutantSummary`; `--output-statuses` filter now uses `s` for skipped
  * Add `--timeout-coefficient N` flag: set per-mutant timeout to N × measured baseline test-suite runtime
  * Add `--git-diff-base <ref>` flag: skip mutation if the source file does not appear in `git diff --name-only <ref>`
  * Include `skipped` field in `--logger-json` output

## [0.4.11]
  * Add `--run-mutant-id <id>` flag: evaluate only the mutant with the given stable ID; skips aggregate summary and exit-policy gates
  * Add `--blacklist <file>` flag: suppress mutations whose ID appears in a file (for permanently excluding false-positive mutants)
  * Print a compact unified diff for each mutant by default; `--no-diffs` suppresses it; `--verbose` still shows the full source after the diff
  * Add inline comment annotations: `-- mucheck: disable-next-line` (all mutators) or `-- mucheck: disable-next-line name1,name2` (named mutators) suppress mutations on the following source line
  * Add CI status badges (Mutation, HLint, OSV-Scanner, Docs, License) to README

## [0.4.10]
  * Add `--logger-json <file>` flag: write a compact JSON summary (total, killed, alive, errors, MSI) to a file after each run
  * Include `covered_code_msi` in `--logger-json` output when a `-tix` file is provided
  * Add `--baseline <file>` flag: skip mutants whose stable ID appears in a file from a previous run
  * Add `--update-baseline <file>` flag: write the IDs of surviving mutants to a file after the run
  * Add `.worktrees` to `.gitignore`; fix escaped-newline formatting bug in `.gitignore`

## [0.4.9]
  * Fix bug in `PrimChar` mutation where it incorrectly used the `Char` constructor
  * Refine `replace-mutable-arg` mutator to avoid matching common single-letter variables like `r`, `m`, and `t`
  * Fix compilation warnings regarding partial functions (`head`), non-exhaustive patterns, and redundant `Typeable` deriving
  * Refactor `stopFast` to stop on the first interpreter error, preventing redundant attempts
  * Improve `fullSummary` and `summarizeResults` to robustly handle multiple test results without relying on `last`

## [0.4.8]
  * Add Dependabot configuration for GitHub Actions and Cabal dependencies
  * Add an OSV-Scanner vulnerability scanning workflow to CI
  * Add a code formatting gate using Fourmolu to CI
  * Build and deploy a Haddock documentation site to GitHub Pages
  * Add a cyclomatic complexity gate using Homplexity to CI
  * Add an MSI quality gate to CI that fails the build if the project's own mutation score drops below 50%
  * Audit and remove dead fields (`_maOriginalNumMutants`) in `MAnalysisSummary`
  * Add generated report artifacts to `.gitignore`
  * Add a link to the deployed documentation site in the `README`

## [0.4.7] (Jonathan Baldie)
  * Add `Data.Bits` operators (`.&.`, `.|.`, `xor`, `shiftL`, `shiftR`, `complement`) to the configurable symbol operator groups
  * Skip mutations whose application site falls inside a type signature, class head, or instance head to avoid generating non-compilable mutants
  * Add `--min-covered-msi <pct>` flag: exit with code 5 if the covered-code MSI is below the threshold
  * Add `--ignore-msi-with-no-mutations` flag: treat MSI quality gates as passed when no mutable constructs are found
  * Add `--timeout N` flag: kill mutant evaluation after N seconds
  * Add `--quiet` flag: suppress output for killed and errored mutants; show only alive mutants
  * Add `--verbose` flag: print per-mutant evaluation details (mutant source, test output)
  * Add `--debug` flag: print mutant stable IDs and raw interpreter diagnostics during a run
  * Add `--no-diffs` flag: suppress the per-mutant unified diff output
  * Add `--output-statuses <chars>` flag: filter terminal output to specific result types (`k`, `a`, `e`, `s`)

## [0.4.6] (Jonathan Baldie)
  * Add `flip-either` mutator: flip between `Right x` and `Left x`
  * Add `remove-forkIO` mutator: strip `forkIO`, `async`, and `withAsync` concurrency wrappers
  * Add `bracket-degenerate` mutator: replace `bracket` with `acquire >>= action`, removing cleanup
  * Add `error-guard` mutator: replace exception handlers (`catch`, `handle`, `try`) with no-ops
  * Add `replace-mutable-arg` mutator: replace mutable variables (`ref`, `mvar`, `tvar`) with `undefined`
  * Enable several GHC language extensions by default in `getASTFromStr` (e.g. `ScopedTypeVariables`, `GADTs`, `TypeFamilies`)

## [0.4.5] (Jonathan Baldie)
  * Add `remove-self-assign` mutator: remove `let x = x` and `x <- return x` self-assignments
  * Add `negate-literal` mutator: replace positive numeric literals with their negation
  * Add `string-literal` mutator: replace non-empty string literals in comparisons with `""`
  * Add `bool-operand` mutator: replace operands in `&&` and `||` with `True`/`False`
  * Add `flip-maybe` mutator: flip between `Just x` and `Nothing`
  * Clean up redundant imports in `Mutation.hs`

## [0.4.4] (Jonathan Baldie)
  * Add `case-alt-remove` mutator: remove one alternative at a time from `case...of` expressions
  * Add `case-default-remove` mutator: remove the catch-all `_` or `otherwise` alternative from `case...of` and guards
  * Add `remove-stmt` mutator: remove one statement at a time from `do`-blocks
  * Add `remove-let-binding` mutator: remove individual bindings from `let...in` and `do`-block `let` statements
  * Add `remove-where-binding` mutator: remove individual bindings from `where` clauses
  * Fix AST traversal to reach bindings in nested constructs
  * Add `Stmt`, `Alt`, and `Rhs` support to the `MuOp` framework

## [0.4.3] (Jonathan Baldie)
  * Add `remove-not` mutator: strip `not` from negated sub-expressions
  * Add `remove-negation` mutator: strip `negate` and prefix `-` from expressions
  * Deduplicate identical mutants (same prettyPrint output) before evaluation to avoid redundant test runs
  * Add `--disable NAME` and `--enable NAME` flags for selective mutation; trailing `*` wildcard supported
  * Add hspec tests for `selectRemoveNotOps` and `selectRemoveNegationOps`

## [0.4.2] (Jonathan Baldie)
  * Add `--fail-on-escaped` flag: exit with code 4 if any mutant survives all tests
  * Add `--min-msi PCT` flag: exit with code 5 if MSI falls below the given percentage threshold
  * Add `--noop` flag: run tests on unmodified source before mutation begins; exit with code 3 if they fail
  * Define and document exit codes: 0=pass, 2=bad arguments, 3=noop failure, 4=escaped mutants, 5=MSI below threshold; unknown flags now exit 2 instead of crashing
  * Print per-mutator breakdown table (killed / alive / errors per MuVar variant) after every run summary

## [0.4.1] (Jonathan Baldie)
  * Update `.cabal` metadata: homepage, maintainer, and source-repository stanzas to point to the jonbaldie/mucheck fork
  * Print MSI (killed ÷ (killed + alive)) as a percentage as the top-line metric in the final summary
  * Count file-write errors during mutant creation in `_maErrors`; never silently discard a mutant that could not be written to disk
  * Skip mutations that produce an AST identical to the original after `prettyPrint` to eliminate false no-op escapes
  * Add `--dry-run` flag: print a per-mutator count of all mutations that would be generated without evaluating any

## [0.4.0.1] (Rahul Gopinath)
  * Make mucheck compilable with latest Haskell libraries

## [0.4.0.0] (Rahul Gopinath)
  * Fix function mutation by making it similar to other mutation operators.
  * Allow users to specify their own groups of interchangeable functions
  * Use test annotations to get tets
  * Make testing module based.
  * Use a better datatype so that we can infer the summarization function to use.
  * Samples on mutants rather than mutation operators
  * Extracts the location of mutation for later use.
  * Add more test cases for mutations
  * Read the HPC Tix files, and use them to filter out unused mutants.
  * Failfast when we fail not when we err.
  * Add a sample adapter

## [0.3.0.0] (Rahul Gopinath)
  * Heavy refacoring, now summary is based on mutations rather than tests.
  * Mutation fails fast on either load errors or test failures

## [0.2.1.1] (Rahul Gopinath)
  * Make it usable from d-mucheck
  * Include logfile name for captured output (or the log string otherwise).

## [0.2.1.0] (Rahul Gopinath)
  * Remove need to pass in module name
  * Add new literal mutators including booleans
  * Refactor Mutation so that IO and Rand are pushed further out, and a sampler is introduced that user can override.
  * Fix the bug where maxMutants were just the first n mutants, rather than n being randomly sampled.
  * Move writefile to front where more choices about it can be made.
  * We no longer require modulename to be passed in.
  * Fix bug in replaceFst

## [0.2.0.0] (Rahul Gopinath)
  * Better documentation
  * Add tests for SYB once
  * Split it to a library and adapter modules
  * Simplify adapter interface
  * Capture stdout and stderr of test runs.

## [0.1.3.0] (Rahul Gopinath)
  * Migrate mucheck to Test namespace
  * Add more tests
  * Fix eamples in docs

## [0.1.2.2] (Rahul Gopinath)
  * Handle single pattern function definitions correctly.

## [0.1.2.1] (Rahul Gopinath)
  * Fix minor tagging issue

## [0.1.2.0] (Rahul Gopinath)
  * Update usage docs
  * Add hspec test framework support
  * Add docs on some functions
  * Refactor summarizable to require only two functions
  * Changes to allow random sampling of mutant categories
  * Refactor more functions to common utils
  * Add a changelog
  * Add a todo

## [0.1.1.0] (Rahul Gopinath)
  * Add command line args
  * Add support for executable in cabal
  * Update docs
  * Add hspec tests for common utils
  * Refactor common utils
  * Better description in cabal
  * Better summarizable class for adding new test frameworks

## [0.1.0.1] (Rahul Gopinath)
  * Refactor the entire project
  * Add tests
  * Add docs
  * Fix null check in ops
  * Add support for functions with no parameter and operators (Onetwogoo)

## [0.1.0.0]
  * Added main, examples, mucheck.cabal,readme (Rahul Gopinath)
  * Added HUnit support (Duc Lee)

## [0.0.1.0] 2013-10-11
  * Initial implementation (Duc Lee)
