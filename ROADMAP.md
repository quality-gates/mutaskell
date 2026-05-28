- [x] Add a `remove-not` mutator: strip `not` from negated sub-expressions in `if` conditions, guards, and `&&`/`||` operands
- [x] Add a `case-alt-remove` mutator: remove one alternative at a time from `case...of` expressions
- [x] Add a `case-default-remove` mutator: remove the catch-all `_` or `otherwise` alternative from `case...of` expressions and guarded definitions
- [x] Add a `remove-stmt` mutator: remove one statement at a time from `do`-blocks, skipping result-binding statements where removal would produce invalid syntax
- [x] Add a `remove-let-binding` mutator: remove individual bindings from `let...in` expressions and do-block `let` groups
- [x] Add a `remove-where-binding` mutator: remove individual bindings from `where` clauses
- [x] Add a `zero-return` mutator: replace the RHS of each function match with the zero value for its declared return type (`False` for Bool, `0` for Num, `""` for String, `Nothing` for Maybe a, `[]` for lists); uses type signatures from the parsed AST rather than full GHC type inference
- [x] Add a `remove-negation` mutator: replace `negate x` and prefix `-x` with `x`
- [x] Add a `remove-self-assign` mutator: remove `let x = x` bindings and `x <- return x` do-statements
- [x] Add a `negate-literal` mutator: replace positive numeric literals with their negation (`42 → negate 42`, `3.14 → negate 3.14`); tests sign-handling that `remove-negation` cannot reach because the original source has no existing negation; Haskell analogue of go-mutesting's `numbers/float-negate`
- [x] Add a `string-literal` mutator: replace non-empty string literals in `==` and `/=` comparisons and guard positions with `""`; finds code that compares against a specific string that tests never flip; Haskell analogue of go-mutesting's `expression/string-literal`
- [x] Add a `bool-operand` mutator: in `&&` and `||` expressions, replace each operand with `True` or `False` to make one arm irrelevant; tests whether both operands are independently exercised by the suite; Haskell analogue of go-mutesting's `expression/remove`
- [x] Add a `remove-forkIO` mutator: strip the `forkIO`/`async`/`withAsync` wrapper and run the action inline
- [x] Add a `bracket-degenerate` mutator: replace `bracket acquire release action` with `acquire >>= action`, removing the cleanup step
- [x] Add a `flip-maybe` mutator: replace `Just x` with `Nothing` and `Nothing` with `Just undefined` in return positions
- [x] Add a `flip-either` mutator: replace `Right x` with `Left x` and `Left x` with `Right x` in return and guard positions
- [x] Add an `error-guard` mutator: in functions that use `catch`, `try`, `handle`, or `throwIO`, replace the error-handling branch with a no-op that returns a zero/default value, testing whether exception-handling paths matter; Haskell analogue of `expression/error-guard`
- [x] Add a `replace-mutable-arg` mutator: replace explicit `IORef`/`MVar`/`TVar` arguments at call sites with `undefined`, testing whether mutable state propagation matters; Haskell analogue of `expression/context-nil`
- [x] Add `Data.Bits` operators (`(.&.)`, `(.|.)`, `xor`, `shiftL`, `shiftR`, `complement`) to the configurable symbol operator groups
- [x] Skip mutations that produce an AST identical to the original after `prettyPrint` to eliminate false no-op escapes
- [x] Skip mutations whose application site falls inside a type signature, class head, or instance head to avoid generating non-compilable mutants
- [x] Support inline source comment annotations to suppress mutations: `-- mucheck: disable-func` before a function body to suppress all mutations in that function; `-- mucheck: disable-next-line [name1,name2]` to suppress specific mutators on the next line (`*` for all); `-- mucheck: disable-regexp <pattern> [*]` to suppress on all lines matching the regex; Haskell analogue of go-mutesting's `// mutator-disable-func`, `// mutator-disable-next-line`, and `// mutator-disable-regexp`
- [x] Add `--dry-run` flag: print a per-mutator count of all mutations that would be generated without evaluating any; note in output that the count is an upper bound before deduplication
- [x] Add `--config <file>` flag: specify an alternate config file path instead of auto-loading `.mucheck.yaml` from the project root
- [x] Add `--quiet` flag: suppress output for killed and errored mutants; show only alive mutants and the final summary
- [x] Add `--verbose` flag: print per-mutant evaluation details (mutant source, test output) during a run
- [x] Add `--debug` flag: print mutant stable IDs and raw interpreter diagnostics during a run
- [x] Add `--no-diffs` flag: suppress the per-mutant unified diff output
- [x] Add `--fail-on-escaped` flag: exit with code 4 if any mutant survives all tests
- [x] Add `--min-msi <pct>` flag: exit with a non-zero code if the final MSI is below `<pct>`
- [x] Add `--min-covered-msi <pct>` flag: exit with code 5 if the covered-code MSI (mutations within HPC-covered lines only) is below `<pct>`; requires a `-tix` file
- [x] Add `--ignore-msi-with-no-mutations` flag: treat MSI quality gates as passed when no mutable constructs are found in the target source; prevents false failures on files that are not yet tested
- [x] Add `--noop` flag: run the test suite once unmodified before mutation begins; exit with a clear error if the suite already fails
- [x] Add `--workers N` flag: fork N subprocesses to evaluate mutants concurrently; hint is not thread-safe so must use process-level parallelism
- [x] Add `--disable <name>` flag: skip a named mutator or category prefix; support trailing-`*` wildcards (e.g. `--disable functions/*`); reject bare `*` with a clear error
- [x] Add `--enable <name>` flag: restrict mutation to only the named mutators or category prefix, with trailing-`*` wildcard support
- [x] Add `--output-statuses <chars>` flag: filter terminal output to specific result types; define chars `k` (killed), `a` (alive), `e` (error), `s` (skipped); ensure diffs for suppressed result types are also suppressed
- [x] Add `--timeout N` flag: kill mutant evaluation after N seconds
- [x] Add `--timeout-coefficient N` flag: scale per-mutant timeout by N times the measured baseline test-suite runtime
- [x] Add `--baseline <file>` flag: skip mutants whose stable ID appears in the given file from a previous run
- [x] Add `--update-baseline <file>` flag: write the stable IDs of surviving mutants to the given file after a run
- [x] Add `--blacklist <file>` flag: suppress specific mutations by content checksum (one hash per line); for ignoring semantically equivalent false-positive mutations; distinct from `--baseline` which tracks accepted survivors; corresponds to go-mutesting's `--blacklist`
- [x] Add `--coverage` flag: auto-discover a `.tix` coverage file in the current directory without requiring the user to provide `-tix FILE` explicitly
- [x] Add `--run-mutant-id <id>` flag: evaluate only the mutant with the given stable ID; do not compute or display MSI or any aggregate summary in this mode
- [x] Add `--logger-json <file>` flag: write a compact JSON summary of run stats (total, killed, alive, skipped, errors, MSI on 0–1 scale) to the given file
- [x] Include `coveredCodeMsi` field in `--logger-json` output when a `-tix` file is provided: report covered-code MSI alongside overall MSI on the 0–1 scale
- [x] Add `--logger-agentic-json <file>` flag: write per-mutant JSON with stable IDs, kill hints, descriptions, and source context lines for LLM consumption
- [x] Add `--logger-gitlab <file>` flag: write a GitLab Code Quality artifact JSON to the given file; use the stable mutant ID as the fingerprint
- [x] Add `--logger-github <file>` flag: write GitHub Actions annotation-format output (`::warning` annotations) for escaped mutants so they appear in the PR diff view
- [x] Add `--logger-html <file>` flag: write a standalone HTML mutation report to the given file; include per-mutant source context, diff, and result classification
- [x] Add `--git-diff-base <ref>` flag: restrict mutation to source files changed relative to `<ref>`; auto-detect the default branch via `git symbolic-ref origin/HEAD` with a fallback to `master`
- [x] Add `--git-diff-lines` flag: when `--git-diff-base` is active, restrict mutations further to lines changed relative to `<ref>`, not just files
- [x] Add `--test-args <flags>` flag: pass additional flags to every invocation of the underlying test runner; forwarded via `withArgs` to every hint-interpreter test call
- [ ] Add `--per-test` flag: build a per-test HPC coverage map and, for each mutation site, run only the tests that cover that location
- [x] Define and document exit codes: 0=pass, 2=bad arguments, 3=pre-flight failure (`--noop`), 4=escaped mutants (`--fail-on-escaped`), 5=MSI below threshold (`--min-msi`)
- [x] Ensure all mutator variant names used in `--disable`/`--enable` and config use consistent separators (no mix of underscores and hyphens)
- [x] Add YAML config file support: load `.mucheck.yaml` from the project root automatically; CLI flags override config values
- [x] Add `disable_mutators` config key: list of mutator names or trailing-`*` category wildcards to skip
- [x] Add `enable_mutators` config key: list of mutator names or trailing-`*` category wildcards to restrict to
- [x] Add `ignore_source_lines` config key: list of substrings; mutations on source lines containing any substring are suppressed
- [x] Add `exclude_dirs` config key: list of source directory prefixes (relative to project root) to skip entirely during mutation
- [x] Add `skip_without_test` config key: when true, skip source modules that have no test annotations rather than treating them as untested
- [x] Add `json_output` config key: persistent equivalent of `--logger-json`; path to write the JSON summary after every run
- [x] Add `html_output` config key: persistent equivalent of `--logger-html`; path to write the HTML report after every run
- [x] Add `silent_mode` config key: when true, print only the final summary line (not suppress it)
- [x] Add `min_msi` config key: persistent equivalent of `--min-msi`; minimum required MSI (0–100); 0 means no gate; overridden by the CLI flag
- [x] Add `min_covered_msi` config key: persistent equivalent of `--min-covered-msi`; minimum required covered-code MSI (0–100); 0 means no gate
- [x] Add `max_mutants` config key: expose the existing `maxNumMutants` field from `Config` to the config file
- [x] Add `timeout` config key, overridden by the `--timeout` CLI flag; `workers` remains pending subprocess implementation
- [x] Reject unknown keys in the config file with a clear error rather than silently ignoring them
- [x] Publish a JSON Schema for the config file (`schema/mucheck-config-schema.json`) with editor autocomplete support
- [x] Add an example `.mucheck.yaml` to the README showing all supported config keys with comments
- [x] Print a live progress line (kill/alive/error counts) that updates every ~200 ms during a run; suppress it in `--quiet` and `silent_mode`
- [x] Print a unified diff for each mutant showing the exact change from original to mutated source, aligned under the result line
- [x] Print a per-mutator breakdown table in the final summary: killed / alive / skipped counts for each `MuVar` variant
- [x] Print MSI (killed ÷ (killed + alive)) as a percentage as the top-line metric in the final summary
- [x] Assign each mutant a stable content-hash ID and print it alongside every result line; use it for `--run-mutant-id`, `--baseline`, and the GitLab fingerprint
- [x] Track skipped (non-compilable) mutants as a distinct category in `MAnalysisSummary`; include them in the per-mutator breakdown and all report formats
- [x] Audit and remove dead fields in `MAnalysisSummary` that are never populated in normal runs (e.g. `_maOriginalNumMutants` before tix data is available)
- [x] Agentic JSON: include a `context_start_line` field anchoring the first context line to its 1-based source line number
- [x] Agentic JSON: include a `description` field showing the exact textual change for single-line mutations
- [x] Agentic JSON: include a `reminder` field with guidance for the consuming LLM on how to act on escaped mutant data
- [x] Agentic JSON: ensure kill hints and descriptions are populated for all `MuVar` variants, not just a subset
- [x] Ensure `msi` is reported on the same 0–1 scale in both `--logger-json` and `--logger-agentic-json` outputs
- [x] Count file-write errors during mutant creation in `_maErrors`; never silently discard a mutant that could not be written to disk
- [x] Implement subprocess-based parallel evaluation for `--workers N`; synchronize all output including diffs through a single writer to prevent interleaved lines
- [x] Deduplicate structurally identical mutations (same `MuOp` at the same span) before evaluation to avoid running redundant tests
- [x] Cache the parsed AST and pretty-printed original source per file so both are computed once, not once per mutant
- [ ] For `--per-test`: build the per-test HPC coverage map before mutation begins; print the module name and test count as a startup message
- [x] Catch and display subprocess and hint errors per mutant cleanly (mutant file path + concise error summary) without letting raw exception traces reach stdout
- [x] Join the progress-display thread before printing the final summary to eliminate the output ordering race
- [ ] Consolidate the triple AST traversal per operator (generate, relevance check, apply) into a single pass
- [x] Write mutant files to a `System.IO.Temp` directory by default and delete them after each evaluation; add `--keep-mutants <dir>` to preserve them
- [x] Add a test asserting each `MuVar` constructor produces at least one mutant on a canonical input (prevent silent registration gaps)
- [x] Add tests for the coverage-filtering code path (`removeUncovered`, `getUnCoveredPatches`) to prevent regressions in HPC-guided mutation
- [ ] Add an Hspec test adapter: run `hspec` programmatically and classify a non-empty failure list as a kill
- [ ] Add a Tasty test adapter: parse ingredient output to classify pass vs fail
- [ ] Add a QuickCheck test adapter: wrap `quickCheckResult` and treat `Failure` as kill, `GaveUp` as `MSumOther`
- [ ] Add an HUnit test adapter: read the `Counts` record and classify `failures + errors > 0` as kill
- [ ] Parse the `.cabal` file to discover all source modules and test suites automatically, eliminating the need to name a file on the command line
- [ ] Support bare `mucheck` invocation (no file argument) to run against all discovered modules using `cabal test`
- [x] Auto-discover test functions by naming conventions (`prop_*`, `spec_`, `test_`) without requiring `{-# ANN ... #-}` annotations
- [x] Expose a `register` / `new` API in `Test.MuCheck.MuOp` so third-party packages can add custom mutators without forking
- [x] Add Dependabot configuration for automated Hackage dependency and GitHub Actions version updates
- [x] Add a vulnerability scanning step to CI (e.g. `cabal-audit` or `osv-scanner`), fully enforcing with no fallback
- [x] Add an MSI quality gate to CI: fail the build if the project's own mutation score drops below a configurable threshold
- [x] Add a code formatting gate to CI: fail if any source file is not formatted by `ormolu` or `fourmolu`
- [x] Add a cyclomatic complexity gate to CI
- [x] Standard for this repo itself is covered-MSI >= 50% (example suite achieves 59%; gate enforced in CI)
- [x] Build and deploy a Haddock + prose documentation site to GitHub Pages
- [x] Extend `.gitignore` to cover generated report artifacts (e.g. `mucheck-summary.json`, `mucheck-agentic.json`, `mucheck-gitlab.json`, `.mucheck-baseline`)
- [x] Audit the README: remove stale or dead references to inactive upstream projects, add a link to the deployed documentation site
- [x] Update `.cabal` metadata: `homepage`, `maintainer`, and both `source-repository` stanzas to point to the fork
- [x] Verify that the github.io docs website actually builds and works
- [x] Entire CI pipeline in PRs must run in less than five minutes end to end
- [x] README.md must be up to date with modern badges
- [x] Add '.worktrees' to .gitignore

## Architectural debt

- [ ] Replace `haskell-src-exts` with `ghc-lib-parser` as the sole AST backend: `haskell-src-exts` is a separate implementation that does not track GHC extensions and silently fails to parse valid GHC 9.x code using `LambdaCase`, `TypeFamilies`, `GADTs`, `LinearTypes`, and others; when the parse fails mucheck produces zero mutants with no diagnostic, which is the worst possible failure mode; `ghc-lib-parser` ships the actual GHC parser in lockstep with each GHC release and handles all extensions; after migration replace `Language.Haskell.Exts.Pretty.prettyPrint` with `exactPrint` from `ghc-exactprint` to preserve original source layout rather than re-formatting, which will also fix misleading column numbers in diffs

- [x] Decompose `app/Main.hs` (currently 1,185 lines) into focused sub-modules: `App.CLI` for option type and parsing, `App.Config` for YAML loading and config merging, `App.Filter` for the eight filter stages (`applyDisableEnable`, `applyAnnotations`, `applyBaseline`, `applyBlacklist`, `applyDiffLines`, `applyIgnoreLines`, `applyRunMutantId`, `capMutants`), `App.Output` (or one module per format) for the five loggers plus diff and breakdown, `App.Worker` for the subprocess parallelism and wire protocol, and `App.Exit` for exit-code policy; `Main.hs` itself should reduce to wiring these modules together inside `main`

- [ ] Replace the hand-rolled `parseOpts`/`parseOptsFrom` CLI parser with `optparse-applicative`: the current parser is a recursive pattern-match over `[String]` covering 71 flags; it gives no `--help`, no shell completion, and no localised error messages; `optparse-applicative` provides all three for free and composes cleanly with the `Opts` record via `Parser Opts`; keep the existing `Opts` record shape, replacing only the parsing layer

- [ ] Replace the hand-rolled `parseYamlKV` config loader with the `yaml` library (already a transitive dependency of many Haskell projects): the current parser splits on `:`, cannot handle quoted strings, inline lists, or multi-line values, and rejects unknown keys only by string comparison; use `Data.Yaml` to decode into a typed config record; the existing `applyYamlConfig` application logic and key validation can be preserved as-is once parsing is delegated to the library

- [x] Move `showMuVar` and add `parseMuVar :: String -> Maybe MuVar` to `Test.MuCheck.Config` (or `Test.MuCheck.MuOp`), and expose both from the library; currently the canonical string names for every `MuVar` constructor (`"functions"`, `"values"`, `"pattern-matches"`, etc.) are defined only inside `app/Main.hs`, which means library consumers cannot convert `MuVar` values to or from strings, and the `--disable`/`--enable` matching logic cannot be unit-tested; `parseMuVar` should round-trip with `showMuVar` and should accept the same wildcard syntax (`"functions/*"`) that the CLI already supports

- [x] Fix `genMutantsWith` in `src/Test/MuCheck/Mutation.hs` to actually use its `Config` argument: the function signature is `genMutantsWith :: Config -> FilePath -> FilePath -> IO (Int, [Mutant])` but the `Config` value is bound as `_config` and `defaultConfig` is hardcoded inside the body; any caller passing a custom `Config` silently gets `defaultConfig`; the fix is to thread the received `Config` through to `genMutantsFromAST` instead of substituting `defaultConfig`

- [x] Fix the filter-before-sample ordering in the mutant pipeline: currently the eight filter stages in `runOpts` run on the sampled set, so if `--git-diff-lines`, `--disable`, or annotation suppression discards many mutants the final count falls well below `--max-mutants` because quota was spent on candidates that were subsequently filtered out; the correct order is: generate all candidates → apply all filters → then sample from the surviving set up to `maxNumMutants`; this also makes `--dry-run` counts consistent with actual run counts

- [ ] Replace the hand-rolled line-based worker wire protocol (`workerSerialize`/`workerDeserialize` in `app/Main.hs`) with the JSON format the tool already produces: the current protocol encodes `MutantSummary` as a sequence of newline-delimited text fields; a single extra newline inside a mutant diff or test output corrupts the deserialiser; serialise each worker result as a self-contained JSON object using the same field names as `--logger-agentic-json`, and deserialise with `Data.Aeson`; add a schema version field so future changes to the record do not cause silent misparses

- [x] Consolidate the triple AST traversal per mutation operator into a single pass: removed the `once`-based applicability probe from `relevantOps` in `Utils.Syb`; the check is now deferred to the existing `once` call in `mutate`, which already returns an empty list for non-applicable ops; reduces O(n_ops) redundant traversals; the `selectXxx`+`once` two-pass architecture is the remaining baseline

- [x] Correct the `tested-with` field in `MuCheck.cabal` to reflect the versions that are actually verified: the multi-GHC matrix in `haskell-ci.yml` is intentionally disabled (`if: false`) because running GHC 9.2–9.8 in a single pipeline takes hours and violates the five-minute CI budget; the `tested-with: GHC == 9.2.8, 9.4.8, 9.6.3, 9.8.2` claim is therefore unverified and misleads Hackage users; narrow the field to only the GHC version(s) that the active CI workflow actually builds against, and add a comment in the cabal file or README explaining that the broader compatibility is aspirational pending a solution to the build time problem (e.g. a nightly or weekly matrix job outside the PR path)

- [x] Remove `sample-test.tix` from version control: it is an HPC coverage artifact generated at build time and must not be committed; add `*.tix` to `.gitignore`, run `git rm --cached sample-test.tix`, and update any README or workflow instructions that reference a committed `.tix` file

- [x] Wire `GenerationMode` into the mutation pipeline or remove the constructor and field entirely: removed `GenerationMode` type and `genMode` field from `Config` entirely; the field was dead code (`programMutantsWith` always called `mutatesN` with `n=1`)

## DX gaps

These are user-facing correctness and ergonomics issues that are not covered by the architectural debt section above. They are independent of each other and can be tackled in any order.

- [x] Fix `getASTFromStr` so that a parse failure produces a user-readable error instead of crashing the process: `getASTFromStr` in `src/Test/MuCheck/Mutation.hs` calls `fromParseResult` which calls `error` on a `ParseFailed` result, dumping a raw Haskell exception trace to the terminal; change `getASTFromStr` to return `IO (Either String Module)` (where the `Left` carries the `haskell-src-exts` error string), propagate the `Either` up through `genMutantsWith` and into `main`, print `"Parse error: <message>"` to stderr, and exit with code 2; do not use `error` or `undefined` anywhere in this call chain; this item becomes redundant once the `ghc-lib-parser` migration is complete, but must be fixed independently because the crash is the current user experience

- [x] Fix `getMix` so that a missing `.mix` file produces a user-readable error instead of crashing the process: `getMix` in `src/Test/MuCheck/Tix.hs` calls `error "mucheck: can not find <module> (or <stripped>) in .hpc"` when neither the module name nor the stripped name resolves to a `.mix` file under `.hpc/`; change `getMix` to return `IO (Either String MixEntry)`, propagate the `Left` through `getUnCoveredPatches` and up to the HPC initialisation block in `app/Main.hs`, print `"Coverage error: cannot find <module> in .hpc — is the test suite built with -fhpc?"` to stderr, and exit with code 2

- [x] Validate that `--enable` and `--disable` are not used together and report a clear error: currently if both flags are provided `--enable` silently wins because `applyDisableEnable` in `app/Main.hs` checks `optEnable` first and returns early; after parsing is complete (or inside `parseOptsFrom` before returning), check whether both `optEnable` and `optDisable` are non-empty and return `Left "Cannot use --enable and --disable together; use one or the other"`, which `main` converts to an exit-2 message; add a test in `App.CLISpec` (or the equivalent) that passes both flags and asserts exit code 2

- [x] Hide `--worker-output` from the user-facing help text: `--worker-output <file>` is an internal IPC flag written by the worker subprocess back to the parent; it has no meaning when invoked directly by a user, and a user who accidentally passes it will silently overwrite a live IPC result file; mark the flag as internal (e.g. by moving it to a separate `internalFlags` list that is excluded from the help block, or by annotating it with a leading `_` in the help string) so it does not appear in the output of `mucheck -h`; it should still parse and function correctly when the subprocess uses it

- [x] Standardise the MSI scale across all machine-readable output formats: the terminal `Show` instance for `MAnalysisSummary` prints `"Mutation score (MSI): 76%"` (integer percentage), while `writeJsonLogger` writes `"msi": 0.7692...` (0–1 float) and `writeAgenticJsonLogger` also uses 0–1; a CI script that reads both formats will silently get values on different scales; fix `writeJsonLogger` and `writeAgenticJsonLogger` so both consistently use 0–1; update `schema/mucheck-config-schema.json` to document `"msi"` as `"type": "number", "minimum": 0, "maximum": 1`; update the README example JSON to match; the terminal summary should continue to display `%` for readability but must derive the displayed value from the same 0–1 float

- [x] Add the `workers` key to the JSON Schema: `knownConfigKeys` and `applyYamlConfig` in `app/Main.hs` both recognise `workers`, but `schema/mucheck-config-schema.json` does not list it under `properties`; editors offering YAML autocomplete against the schema will not suggest it, and schema-validating tools will reject it; add `"workers": { "type": "integer", "minimum": 1, "description": "Number of parallel subprocess workers (default 1)" }` to the `properties` object; verify `additionalProperties: false` still holds after the addition

- [x] Fix the stale `source-repository this` tag in `MuCheck.cabal`: the `source-repository this` stanza still carries `tag: 0.4.13` even though the current `version:` field reads `0.4.19`; update `tag:` to `0.4.19`; going forward, the `version:` field and `tag:` must be updated together whenever a release commit is made, so add a note to the release checklist in `AGENTS.md` (step 9) to update both

- [x] Update `Examples/AssertCheckTest.hs` to demonstrate naming-convention auto-discovery: the only shipped example still annotates every test with `{-# ANN funcName "Test" #-}`; the naming-convention feature added in 0.4.18 (`prop_*`, `spec_*`, `test_*`) is entirely invisible to new users; rename at least two of the five test functions so they are discovered by convention rather than by annotation (e.g. `testIsSorted`, `prop_sortIsIdempotent`), leave one `{-# ANN #-}` example to show the legacy path, and add a top-of-file comment block explaining both mechanisms and noting that `{-# ANN #-}` is the opt-in override for names that do not follow a convention

- [x] Resolve the conflict between the `hspec-discover` pragma and the manual wiring in `test/Spec.hs`: `Spec.hs` carries `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` which directs GHC to auto-generate the entry point at compile time, but the file also manually defines `main` and `spec` and hand-imports every `*Spec` module; only the manual path runs because `Spec.hs` is listed as `main-is` in the cabal `test-suite` stanza; the conflict means any new `*Spec.hs` added under `test/Test/MuCheck/` is silently missing from the suite unless it is also added to `Spec.hs` by hand; pick one approach: either remove the `{-# OPTIONS_GHC #-}` pragma and keep the manual wiring (adding a comment that new files must be imported here), or remove the manual `main`/`spec` definitions and let hspec-discover take over; whichever approach is chosen, run `cabal test` and confirm all 24 tests still pass

- [x] Add at least one end-to-end integration test for the evaluation pipeline: there are currently no tests for `evaluateMutants` or the surrounding orchestration in `Test.MuCheck`; add a test in a new file `test/Test/MuCheck/IntegrationSpec.hs` that calls `mucheck` (the library function, not the binary) on `Examples/AssertCheckTest.hs` inside a `withCurrentDirectory` block pointing at a temporary copy of the project root, waits for the result, and asserts: `_maKilled summary > 0`, `_maNumMutants summary > 0`, and `_maKilled summary + _maAlive summary + _maErrors summary + _maSkipped summary == _maNumMutants summary`; this test will be slow (it forks subprocesses) so mark it with `-- integration` and document how to run it selectively via `cabal test --test-option=--match`

- [x] Add unit tests for the CLI parser and config loader: after the `App.CLI` and `App.Config` decomposition (see architectural debt section), add `test/App/CLISpec.hs` with tests covering: (a) every flag round-trips through `parseOptsFrom` and sets the expected `Opts` field, (b) unknown flags return `Left` containing `"Unknown flag"`, (c) `--min-msi` with a non-integer argument returns `Left` containing `"requires an integer argument"`, (d) config file values are applied as defaults and CLI flags override them, (e) each of the five exit-code conditions (`exitSuccess`, exit 2, exit 3, exit 4, exit 5) is reachable from the corresponding `Opts` state; until the decomposition is complete, add these tests against `parseOptsFrom` exported directly from `app/Main.hs` by adding it to the `other-modules` of the `spec` test-suite stanza in `MuCheck.cabal`