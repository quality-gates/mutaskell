# Exploratory testing: Configuration, Scoping, and Project Mode

- **Date**: 2026-09-12
- **Build**: branch `main` @ `28b8098`, mutaskell 0.8.33
- **Toolchain**: GHC 9.12.1 (Homebrew), macOS aarch64 (Darwin 25.6.0), 8 CPUs
- **Setup**: `cabal build --write-ghc-environment-files=always all` (clean)
- **Evidence**: `/tmp/et-mutaskell-2026-09-12/` (run logs, reproduction configs, demo scratch project, issue bodies)
- **Interface driven**: the `mutaskell` CLI only. No internal APIs were called directly.

## Baseline health

1. Documented smoke test passed:
   ```console
   cabal run mutaskell -- Examples/AssertCheckTest.hs --workers 1 --timeout 30
   ```
   Result: MSI 92%, 68 mutants (63 killed, 4 alive, 1 skipped, 0 errors).

2. Full test suite passed:
   ```console
   cabal test all --test-show-details=direct
   ```
   Result: 364 examples, 0 failures.

## Journeys exercised

### J1 — Config file workflows (`.mucheck.yaml` and custom `--config`)

Goal: Configure mutaskell project-wide via a configuration file and verify that configuration defaults and gates apply properly, CLI arguments override them, and malformed/missing configuration files are handled gracefully.

Observations:
- Ordinary path: Placing `.mucheck.yaml` in the project root with options like `min_msi`, `quiet`, `timeout`, and mutator lists is loaded and applied to `baseOpts`.
- Unknown keys in configuration files are properly rejected with exit 2 and a list of valid keys.
- Passing `--config <path>` (with a space) correctly loads the specified configuration file.
- **Confirmed bug found: [#79](https://github.com/quality-gates/mutaskell/issues/79)** — Passing `--config=FILE` (standard GNU syntax supported by optparse-applicative) is ignored by `extractConfigArg`. The user's configuration file is never loaded into `baseOpts`, and quality gates (like `min_msi: 100`) are completely bypassed, resulting in silent gate passage (exit 0).
- **Confirmed bug found: [#80](https://github.com/quality-gates/mutaskell/issues/80)** — Specifying a non-existent file via `--config /no/such/file.yaml` is silently ignored (exits 0) instead of reporting an error and exiting 2. `loadConfig` treated any missing file as optional discovery.

### J2 — Targeted and scoped mutation filtering

Goal: Run mutation testing on a restricted scope (such as git diffs, blacklists, inline suppressions, and mutant caps) and verify that only targeted mutations are evaluated.

Observations:
- In single-file mode:
  - `--keep-mutants <dir>` correctly creates the target directory if missing and writes surviving mutant ASTs.
  - `--output-statuses` filters terminal output by result classification (`k`, `a`, `e`, `s`).
  - `--blacklist` and `--baseline` suppress matching mutant hashes (reporting warnings if the file is unreadable).
  - Inline comment annotations (`-- mucheck: disable-next-line`) correctly suppress mutations on the line immediately following the annotation in single-file mode.
- Git diff scoping:
  - Passing an invalid ref to `--git-diff-base` emits git's stderr message, but `checkGitDiff` catches the error and silently falls back to `True`, evaluating all mutants without failing CLI argument validation.

### J3 — Project mode (`mutaskell <dir>`) and whole-repository workflows

Goal: Run mutaskell across an entire cabal package/project directory, driving its real build and test suite, respecting configuration and diff scopes.

Driven against a realistic minimal consumer project (`/tmp/et-mutaskell-2026-09-12/demo`: library + test suite).

Observations:
- Project mode auto-detects cabal project roots, runs baseline build and baseline test suite, and isolates working tree mutations with automatic source restoration.
- **Confirmed bug found: [#81](https://github.com/quality-gates/mutaskell/issues/81)** — Project mode completely ignores `--git-diff-base` and `--git-diff-lines`. Despite being documented on line 27 of `README.md` (`cabal run mutaskell -- --git-diff-base origin/main .`) and in `setups/diff-only.mucheck.yaml` as the recommended way to gate PRs, `App.Project` never checks `optGitDiffBase` or `optGitDiffLines`. It discovers and mutates every file in the entire repository, turning fast PR checks into full-codebase mutation runs.
- **Confirmed bug found: [#82](https://github.com/quality-gates/mutaskell/issues/82)** — Project mode does not apply inline annotations (`-- mucheck: disable-next-line`) or `ignore_source_lines`. In `App.Project` (`processFile'`), only `applyDisableEnable` is applied to generated mutants. Mutants on lines marked with `-- mucheck: disable-next-line` or matching `ignore_source_lines` are evaluated anyway.

## Confirmed findings

| # | Issue | Severity | User impact |
| :- | :--- | :--- | :--- |
| 1 | [#79](https://github.com/quality-gates/mutaskell/issues/79) `--config=FILE` syntax is ignored by CLI parser, causing config and quality gates to be bypassed | High | Users passing `--config=file.yaml` have quality gates silently bypassed (exit 0) |
| 2 | [#80](https://github.com/quality-gates/mutaskell/issues/80) Specifying a non-existent `--config FILE` is silently ignored instead of reporting an error (exit 2) | Medium | Typos in `--config` silently run with default options rather than failing the build |
| 3 | [#81](https://github.com/quality-gates/mutaskell/issues/81) Project mode ignores `--git-diff-base` (and `--git-diff-lines`), mutating all discovered files instead of diff-scoped files | High | Documented PR quickstart (`mutaskell --git-diff-base origin/main .`) runs full repository instead of PR diff |
| 4 | [#82](https://github.com/quality-gates/mutaskell/issues/82) Project mode does not apply inline annotations (`-- mucheck: disable-next-line`) or `ignore_source_lines` | High | Source suppressions work in single-file mode but fail in whole-project/CI runs |

All four findings were deterministically reproduced, replayed from known starting states, and filed in the project tracker with minimal reproducers, impacted source locations, and proposed fixes.

## Rejected candidates

- **`--keep-mutants` fails if the directory does not exist.** Rejected: mutaskell calls `createDirectoryIfMissing True` and successfully creates the directory.
- **`--output-statuses` in project mode.** Rejected as candidate bug: `--output-statuses` is documented as a CLI formatting option for the per-mutant terminal output; project mode uses an orchestrator progress logger instead.
- **Blank lines after `-- mucheck: disable-next-line` not suppressing target code.** Rejected: `README.md` specifies that the annotation must be placed "immediately before the line to skip".

## Unresolved

None. All candidates investigated during the session were classified.

## Unexplored / blocked

- **HPC coverage generation with real `.mix` / `.tix` files.** Generating an actual cabal-instrumented coverage build with GHC 9.12.1 requires setting up a package-level cabal coverage build and mapping `dist-newstyle` mix paths.
- **Time-budget edge cases under heavy build times.** When a single file takes longer than `--time-budget`, the budget check currently happens between files rather than aborting the active file build.

## Usability observations

1. **Missing `--config FILE` vs default `.mucheck.yaml` ambiguity:** The user gets no diagnostic feedback when a non-existent configuration file is passed. An explicit flag should always be verified.
2. **`--git-diff-lines` without `--git-diff-base`:** Passing `--git-diff-lines` on its own does nothing because `applyDiffLinesCached` checks for `Just ref`. A validation error in `validateOpts` (like the one checking `--min-covered-msi` without coverage) would prevent user confusion.

## Cleanup

- Removed all test artifacts and scratch files from the repository root (`.mutaskell/`, `.mutants/`).
- Evidence directory `/tmp/et-mutaskell-2026-09-12/` is preserved with reproducers and run outputs.
