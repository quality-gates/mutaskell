# Exploratory testing: Baseline Updates, Diff Scoping, Path Exclusions, and Targeted Mutant Quality Gates

- **Date**: 2026-10-03
- **Build**: branch `main` @ `922dcfe`, mutaskell 0.8.52
- **Toolchain**: GHC 9.12.1 (Homebrew), macOS aarch64 (Darwin 25.6.0), 8 CPUs
- **Setup**: `cabal build --write-ghc-environment-files=always all` (clean)
- **Evidence**: `/tmp/et-mutaskell-2026-10-03/` (baseline run outputs, reproducer projects, diff tests)
- **Interface driven**: the `mutaskell` CLI only. No internal APIs were called directly.

## Baseline health

1. Documented smoke test passed:
   ```console
   cabal run mutaskell -- Examples/AssertCheckTest.hs --workers 1 --timeout 30
   ```
   Result: MSI 92%, 68 mutants (63 killed, 4 alive, 1 skipped, 0 errors).

2. Test suite verification:
   Individual specs and worker comparison tests passed cleanly (e.g. `Test.Mutaskell.Worker.worker subprocess.workers 1, 2 and 4 agree on outcomes for the same workload [✔]`).

## Journeys exercised

### J1 — Baseline Filtering and Updating Lifecycle

Goal: Record known surviving mutants into a baseline file via `--update-baseline`, filter them out on subsequent runs using `--baseline`, and update the baseline with newly escaping mutants.

Observations:
- Running `--update-baseline /tmp/baseline.txt` on source with 4 escaping mutants writes their 4 stable hashes into `/tmp/baseline.txt`.
- Running `--baseline /tmp/baseline.txt` filters out the 4 surviving mutants, leaving 64 candidates (63 killed, 1 skipped, 0 alive), raising MSI to 98%.
- **Confirmed bug found: [#128](https://github.com/quality-gates/mutaskell/issues/128)** — When `--baseline FILE` and `--update-baseline FILE` are specified together (the documented workflow to record newly escaping mutants while ignoring previously known ones), `--baseline` filters out all previously baselined mutants before evaluation begins (`applyBaselineCached`). During evaluation, none of the previously baselined mutants appear in `tsum`. `writeUpdateBaseline` in `app/App/Output.hs` unconditionally overwrites `FILE` with `writeFile path (unlines aliveIds)`. Because `aliveIds` is empty, `writeFile` truncates `FILE` to 0 bytes, erasing all known baseline mutants. On the subsequent run, all previously baselined mutants re-appear as alive and fail quality gates. Furthermore, running `--update-baseline` on an existing baseline file without `--baseline` overwrites rather than merging new mutants, losing past baselines across multiple files.

### J2 — Source Directory Scoping and Configuration Discovery

Goal: Configure source exclusions in `.mucheck.yaml` via `exclude_dirs` with multi-level directory paths, and verify project-mode discovery behavior.

Observations:
- In single-file mode (`app/Main.hs`), `exclude_dirs` checks `isPrefixOf`, allowing both single components (`sub`) and nested paths (`src/sub`) to skip target files.
- Project mode discovery (`App.Project.discoverSourcesWithStats`) walks discovered source directories and applies `excluded`.
- **Confirmed bug found: [#129](https://github.com/quality-gates/mutaskell/issues/129)** — In `app/App/Project.hs:803`, `excluded` checks `any (`elem` pathParts p) ("dist-newstyle" : optExcludeDirs opts)`. Because `pathParts` splits `p` on `'/'` into single-level directory components (e.g. `"src/sub/B.hs"` produces `["src", "sub", "B.hs"]`), testing whether `"src/sub"` is an element of `["src", "sub", "B.hs"]` always evaluates to `False`. Neither `"src"` nor `"sub"` matches `"src/sub"`. Consequently, any nested directory path configured in `exclude_dirs` (e.g. `src/generated`, `test/fixtures`, `vendor/lib`) is never excluded in project mode.

### J3 — Git Diff Scoping and Failure Handling

Goal: Scope single-file and project runs to modified lines and files relative to a base git reference via `--git-diff-base REF` and `--git-diff-lines`.

Observations:
- When a valid base ref is passed and files have diffs, mutators are restricted to changed lines.
- **Confirmed bug found: [#130](https://github.com/quality-gates/mutaskell/issues/130)** — When `git diff` fails (for example, if `REF` is invalid, mistyped, or not fetched in a shallow CI checkout), `readProcess` raises an `IOException`. In `checkGitDiff`, `loadChangedLines` (`app/App/Filter.hs`), and `filterDiff` (`app/App/Project.hs`), the exception is caught with `try` and silently converted into a fallback that includes everything: `checkGitDiff` returns `True`, `loadChangedLines` returns `Nothing` (causing `applyDiffLinesCached` to keep all lines), and `filterDiff` returns all project files. Git prints `fatal: ambiguous argument '<ref>'` to stderr, but mutaskell ignores the failure, runs a full un-scoped mutation evaluation across the entire repository, and exits 0.

### J4 — Targeted Mutant Evaluation and Quality Gate Policies

Goal: Evaluate a single mutant by ID using `--run-mutant-id ID`, check status, diff output, and verify quality gate enforcement (`--fail-on-escaped`, `--min-msi`).

Observations:
- Running `--run-mutant-id <id>` targets only the specified mutant, printing its progress and diff.
- **Confirmed bug found: [#131](https://github.com/quality-gates/mutaskell/issues/131)** — In `app/Main.hs:251`, `unless (isSingleMutantMode opts) $ do ... applyExitPolicy opts msum` wraps the entire summary and exit policy block. Because `isSingleMutantMode` is `True` whenever `--run-mutant-id` is set, `applyExitPolicy` is completely skipped. Running a targeted mutant check with `mutaskell file.hs --run-mutant-id <id> --fail-on-escaped` or `--min-msi 100` always exits with code 0 even when the mutant survives (`ALIVE`). Furthermore, if `--run-mutant-id` is passed with a non-existent mutant ID, mutaskell outputs nothing and exits 0 as well.

## Confirmed findings

| # | Issue | Severity | User impact |
| :- | :--- | :--- | :--- |
| 1 | [#128](https://github.com/quality-gates/mutaskell/issues/128) `--update-baseline` overwrites baseline file instead of updating/merging, truncating existing baseline to empty when used with `--baseline` | High | Re-running with `--baseline` and `--update-baseline` wipes out all existing baseline entries on green runs, causing all previously baselined mutants to fail gates on subsequent runs |
| 2 | [#129](https://github.com/quality-gates/mutaskell/issues/129) `exclude_dirs` fails to exclude directory paths containing slashes in project mode | High | Configured exclusions for nested directories (`src/sub`, `test/fixtures`, `vendor/lib`) are ignored during project discovery; files in those directories are mutated anyway |
| 3 | [#130](https://github.com/quality-gates/mutaskell/issues/130) `--git-diff-base` silently ignores git failures and falls back to un-scoped whole-project mutation | High | Invalid, mistyped, or un-fetched git refs in CI shallow clones fail silently and trigger full repository mutation runs, exhausting time budgets |
| 4 | [#131](https://github.com/quality-gates/mutaskell/issues/131) `--run-mutant-id` bypasses all quality gate exit policies and exits with code 0 on escaping mutants | Medium | Targeted mutant re-runs with `--fail-on-escaped` or `--min-msi` always exit with code 0 even when the targeted mutant escapes undetected |

All four findings reproduce deterministically from a clean starting state, have minimal reproducers, and were filed in the project issue tracker with root-cause source locations and proposed fixes.

## Rejected candidates

- **`--max-mutants 0` division by zero or crash.** Rejected: Confirmed that `max-mutants 0` safely returns 0% MSI and emits valid JSON with `"msi": 0.0`.
- **`--output-statuses` crashes on unrecognized status characters.** Rejected: Statuses are filtered with `elem`; unrecognized characters cleanly result in no matching mutant details being printed without any crash.

## Unresolved

None. Every candidate investigated during the session was classified with evidence.

## Unexplored / blocked

- **HPC coverage auto-discovery under complex multi-package directory layouts.** Requires compiling multiple packages with `-fhpc` and mapping `.mix` directories across Cabal packages.
- **`--jobs N` (N > 1) parallel worker execution.** Kept at `--jobs 1` in accordance with shared Fleet host rules.

## Usability observations

1. **Project mode ignores project-root `.mucheck.yaml` when invoked from another directory:** When running `mutaskell /path/to/project`, `Main.hs` only checks for `.mucheck.yaml` in the current working directory, not in the target project root. Unless the user explicitly supplies `--config /path/to/project/.mucheck.yaml`, project-local configuration is ignored.
2. **`--run-mutant-id` with non-existent ID produces no feedback:** When `--run-mutant-id` is passed an unknown hash or typo, mutaskell silently prints nothing and exits 0. An explicit error (e.g. `Error: mutant ID '<id>' not found in <file>`) with exit code 2 would clarify why nothing ran.

## Cleanup

- Removed temporary baseline files (`/tmp/et-baseline.txt`, `/tmp/et-baseline-copy.txt`) and demo projects from `/tmp/`.
- Evidence directory `/tmp/et-mutaskell-2026-10-03/` is preserved with reproducers, run logs, and test configs.
