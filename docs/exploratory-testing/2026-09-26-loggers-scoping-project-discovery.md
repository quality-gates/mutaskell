# Exploratory testing: Report Loggers, Timing Calibration, and Project Discovery

- **Date**: 2026-09-26
- **Build**: branch `main` @ `25dcc40`, mutaskell 0.8.44
- **Toolchain**: GHC 9.12.1 (Homebrew), macOS aarch64 (Darwin 25.6.0), 8 CPUs
- **Setup**: `cabal build --write-ghc-environment-files=always all` (clean)
- **Evidence**: `/tmp/et-mutaskell-2026-09-26/` (run logs, logger outputs, reproducer projects, syntax fixture)
- **Interface driven**: the `mutaskell` CLI only. No internal APIs were called directly.

## Baseline health

1. Documented smoke test passed:
   ```console
   cabal run mutaskell -- Examples/AssertCheckTest.hs --workers 1 --timeout 30
   ```
   Result: MSI 92%, 68 mutants (63 killed, 4 alive, 1 skipped, 0 errors).

2. Full test suite passed:
   ```console
   cabal test --enable-tests all --test-show-details=direct
   ```
   Result: 407 examples, 0 failures.

## Journeys exercised

### J1 — Pre-flight Verification, Test Runners, and Dynamic Timeouts

Goal: Verify baseline integrity before mutation, configure test arguments, and calibrate timeouts dynamically via the test suite baseline.

Observations:
- `--noop` on clean source (`Examples/AssertCheckTest.hs`) verifies that the test suite passes on unmodified source, then proceeds with normal mutation evaluation.
- `--noop` on source with a failing test (`/tmp/et-mutaskell-2026-09-26/FailingTest.hs`) immediately fails before generating mutants with `Pre-flight check failed: test suite does not pass on unmodified source` and exits with code 3.
- `--timeout-coefficient N` scales per-mutant timeout by `N` times the measured baseline runtime. In single-file mode, it measures baseline runtime via `evalTest`.
- **Confirmed bug found: [#111](https://github.com/quality-gates/mutaskell/issues/111)** — `--timeout-coefficient` writes captured test stdout/stderr to `.mucheck-baseline-timing.log` in the current working directory. Unlike `.mucheck-noop.log`, this file is not listed in `.gitignore`. Furthermore, neither `resolveTimeout` nor `noopCheck` ever cleans up their log file after completion, leaving the working tree permanently dirty (`git status` shows an untracked file).
- **Confirmed bug found: [#112](https://github.com/quality-gates/mutaskell/issues/112)** — `--test-args ARG` is forwarded to `hint` in single-file mode, but `App.Project` and `App.Orchestrator` never inspect or append `optTestArgs` to the test command (`cabal test all ...`). Test filtering flags passed via `--test-args` are silently ignored in project mode.

### J2 — Artifacts and Report Loggers

Goal: Generate all supported CI report formats (`--logger-json`, `--logger-gitlab`, `--logger-github`, `--logger-agentic-json`, `--logger-html`) and verify their syntax, escaping, structure, and directory handling.

Observations:
- All five loggers successfully write output when their target directory exists:
  - `--logger-json`: Emits valid JSON conforming to the summary schema with `msi` as a normalized 0–1 float.
  - `--logger-gitlab`: Emits valid GitLab Code Quality JSON listing escaped mutants with file paths, begin lines, and fingerprints.
  - `--logger-github`: Emits standard GitHub Actions `::warning file=...,line=...::` annotations for surviving mutants.
  - `--logger-agentic-json`: Emits valid JSON containing the overall summary, per-mutant contexts, descriptions, and diffs (verifying the unicode fix from [#61](https://github.com/quality-gates/mutaskell/issues/61)).
  - `--logger-html`: Emits a standalone HTML report with embedded CSS, diff syntax coloring, and correctly HTML-escaped tokens (`&lt;`, `&gt;`, `&quot;`).
- **Confirmed bug found: [#109](https://github.com/quality-gates/mutaskell/issues/109)** — If an output logger file (or `--update-baseline`) is specified with a path in a directory that does not yet exist (e.g. `--logger-json build/reports/summary.json` or `--logger-html reports/mutaskell.html`), mutaskell evaluates all mutants to completion and then crashes with an unhandled exception: `withFile: does not exist (No such file or directory)` (exit code 1). Unlike `--keep-mutants DIR`, `App.Output` never creates parent directories.

### J3 — Project Mode, Complex Syntax, and Source Discovery

Goal: Test AST mutators against advanced Haskell syntax constructs (records, multi-way if, pattern guards, list comprehensions, do-notation, case alternatives) and verify project-mode source discovery on a real Cabal package.

Observations:
- Exercised `SyntaxExplore.hs` containing records with updates, lambda expressions, multi-way if, pattern guards, list comprehensions, do-notation, and nested case alternatives:
  - 58 mutants generated across 12 distinct mutators (`literal-values`, `functions`, `other:negate-literal`, `other:zero-return`, `negate-guards`, `other:case-default-remove`, `other:pattern-constructor`, `remove-negation`, `other:bind-to-sequence`, `other:case-alt-remove`, `other:flip-maybe`, `other:remove-stmt`).
  - Result: 50 killed, 3 alive, 5 skipped (uncompilable mutants like stripping monadic bindings), 0 errors. MSI 86%. Mutators handle all tested constructs cleanly.
- Project mode on minimal Cabal project (`/tmp/et-mutaskell-2026-09-26/demo`):
  - Normal project mode correctly auto-detects `cabal.project` / `*.cabal`, discovers `src/Lib.hs`, executes baseline build/test, and records survivors in `.mutaskell/survivors.txt`.
- **Confirmed bug found: [#110](https://github.com/quality-gates/mutaskell/issues/110)** — `discoverSourcesWithStats` in `App.Project` lists the project root `.` and only excludes `["dist-newstyle", ".git", ".stack-work"]`. If `.mutants/` exists in the project root (the project's standard `--keep-mutants` directory), mutaskell discovers all `.hs` files in `.mutants/` as project source files. Because these files are not part of the Cabal test suite, every mutant survives (`Alive`), depressing the project's MSI (in our reproducer dropping it from 80% to 40%) and failing quality gates.

## Confirmed findings

| # | Issue | Severity | User impact |
| :- | :--- | :--- | :--- |
| 1 | [#109](https://github.com/quality-gates/mutaskell/issues/109) Missing parent directory for report logger or baseline output causes uncaught IOException after full run | High | Whole mutation evaluation completes, then crashes with exit 1 without writing reports |
| 2 | [#110](https://github.com/quality-gates/mutaskell/issues/110) Project mode discovers and mutates `.mutants/` and hidden directories, depressing mutation score | High | Orphaned mutant files in `.mutants/` are mutated, 100% survive, halving MSI and failing gates |
| 3 | [#111](https://github.com/quality-gates/mutaskell/issues/111) `--timeout-coefficient` creates untracked `.mucheck-baseline-timing.log` and never cleans it up | Medium | Leaves untracked file in working directory on every calibrated run, dirtying git worktree |
| 4 | [#112](https://github.com/quality-gates/mutaskell/issues/112) `--test-args` is silently ignored in project mode and orchestrator mode | Medium | Test suite filtering arguments passed via `--test-args` are silently dropped |

All four findings reproduce deterministically from clean starting state, have minimal reproducers, and were filed in the project issue tracker with root-cause source locations and proposed fixes.

## Rejected candidates

- **`--output-statuses xyz` crashes on invalid character input.** Rejected: `printMutantDetailsWithDiffs` filters statuses by checking `c `elem` optOutputStatuses opts`; invalid characters simply mean no statuses match, causing mutant diff details to be cleanly omitted.
- **`--max-mutants 0` causes division by zero in MSI calculation.** Rejected: `summaryMsi` checks `noerrs > 0` before division and safely returns 0%.
- **Complex AST syntax causes crashes or interpreter errors in mutators.** Rejected: `SyntaxExplore.hs` tested record updates, multi-way if, pattern guards, list comprehensions, do-notation, and nested case alternatives; all 58 mutants were cleanly evaluated (50 killed, 3 alive, 5 skipped, 0 errors).

## Unresolved

None. Every candidate investigated during the session was classified with evidence.

## Unexplored / blocked

- **HPC coverage generation with real `.tix` / `.mix` files.** Still requires setting up a package-level cabal coverage build with matching mix directory mappings.
- **`--jobs N` (N > 1) under heavy filesystem contention.** Tested `--jobs 1` in accordance with shared Fleet host rules.

## Usability observations

1. **`--git-diff-lines` without `--git-diff-base` silently ignored:** The help text notes that `--git-diff-lines` requires `--git-diff-base`. However, passing `--git-diff-lines` alone is accepted without error or warning, and full mutations are evaluated because `applyDiffLinesCached` requires `Just ref`. Adding a check to `validateOpts` would prevent user confusion.
2. **Missing logger directory error format:** When an output file cannot be written due to a missing directory, the user sees an uncaught GHC callstack instead of a clean CLI error. Even if directories are auto-created, IO errors should be reported gracefully.

## Cleanup

- Removed temporary baseline log files (`.mucheck-baseline-timing.log`, `.mucheck-noop.log`) from the repository root.
- Evidence directory `/tmp/et-mutaskell-2026-09-26/` is preserved with reproducers, run logs, and logger outputs.
