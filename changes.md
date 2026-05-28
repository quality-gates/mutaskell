# Changelog

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
