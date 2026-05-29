# mucheck

Mutation testing for Haskell. Parses Haskell source, applies mutations (literal values, operators, pattern matches, guards, boolean conditions), and runs the test suite via the `hint` interpreter to check whether tests detect the change.

## Build & test

```bash
cabal build all
cabal test all --test-show-details=direct
```

All tests in the `spec` suite must pass. GHC 9.12.1 (Homebrew) is the only tested compiler; CI runs GHC 9.12.1 only.

## Key modules

| Module | What it does |
| :--- | :--- |
| `app/Main.hs` | Binary entrypoint; `parseOpts` handles all CLI flags — see `go` cases for the full list |
| `src/Test/MuCheck.hs` | Top-level orchestration: generate → sample → evaluate → summarise |
| `src/Test/MuCheck/Mutation.hs` | All mutant generation: literal ops, bool ops, if/else, guards, function/operator substitution, pattern match permutation |
| `src/Test/MuCheck/MuOp.hs` | `MuOp` type and `Mutable` class; AST type aliases |
| `src/Test/MuCheck/Config.hs` | `Config`, `MuVar`, `FnOp`; default operator groups |
| `src/Test/MuCheck/Interpreter.hs` | hint-based mutant evaluation; `stopFast` short-circuit logic |
| `src/Test/MuCheck/TestAdapter.hs` | `Summarizable` / `TRun` type class interface for test frameworks |
| `src/Test/MuCheck/AnalysisSummary.hs` | `MAnalysisSummary` type and its `Show` instance |
| `src/Test/MuCheck/Tix.hs` | HPC `.tix` / `.mix` parsing for coverage-guided mutation |

## Smoke test

The `hint` interpreter resolves modules at runtime from the GHC package database. Running the bare binary directly fails because `Test.MuCheck.TestAdapter.AssertCheck` is not installed system-wide. The correct procedure is:

```bash
cabal build --write-ghc-environment-files=always all
cabal run mucheck -- Examples/AssertCheckTest.hs
```

The output shows a mutation score, per-mutant results, and a per-mutator breakdown table. Exact counts change as mutators are added or removed. What to check:

- The run completes without a crash.
- At least one mutant is killed (the `functions` row will always have kills).
- The killed count has not dropped compared to the previous run.

Some errors are expected and normal — for example, the `/` operator mutation on `uncoveredDummy` produces `0 / a :: Int`, which does not typecheck. Some mutants will always escape because the example tests are not exhaustive.

## Shipping workflow

Follow these steps in order when landing a change:

1. **Build and test locally** — `cabal build all` and `cabal test all --test-show-details=direct`. All tests must pass.
2. **Run the smoke test** — build with `--write-ghc-environment-files=always`, then `cabal run mucheck -- Examples/AssertCheckTest.hs`. Confirm the kill count has not dropped.
3. **Run haddock** — `cabal haddock all`. Confirm no new undocumented export warnings.
4. **Update docs if needed** — if your change adds, removes, or renames a mutator, flag, config key, or user-facing behaviour, update `README.md` to match before committing.
5. **Update changes.md** — add an entry describing what changed (Added / Fixed / Changed). Keep entries concise.
6. **Bump the version in the PR** — pick the next semver tag and update the `version:` field and `source-repository this` tag in `MuCheck.cabal` *before opening the PR*. This must land in the same PR as the change, not after. Verify with `git diff $(git describe --tags --abbrev=0)..HEAD -- MuCheck.cabal | grep version` that the version moves forward, not backward. If the bump is deferred until after merge, branches cut from master for follow-up fixes will start from the old version and silently revert it when they land.
7. **Commit and push** — fix forward only. No `--force-push` and no `--amend` on published commits. Do not use `reset --hard` either. Do not ask the user to run destructive git commands for you. If a hook or check fails, fix it in a new commit. **The `master` branch has push protection — all changes must land via a PR.**
8. **Watch CI** — wait for the Actions run to go green before merging. Run `gh pr checks <number>` to confirm every workflow passes; do not merge if any is red. Also check for inline code-scanning comments (HLint posts findings as PR review comments); fix any warnings before merging. Use `gh api repos/jonbaldie/mucheck/pulls/<number>/comments --jq '.[].body'` to list them.
9. **Merge to master** — squash or merge commit, then push master. Ensure local master is in sync with new origin/master.
10. **Tag and release** — apply the semver tag chosen in step 6 to the merge commit. Create a GitHub release: succinct style, plain English, list what changed.

## Conventions

- **Edit files one at a time using Read then Edit.** Do not use scripts or bulk replacements across multiple files at once. Small differences between files (naming, existing imports, extra test functions) mean a bulk approach produces inconsistent output that must be cleaned up manually.
- The `hint` interpreter is **not thread-safe**. Parallel mutation evaluation must use forked subprocesses, not `forkIO` or `async` within a single process.
- HLint is configured in `.hlint.yaml` and enforced by CI against `src app Examples`. Run `hlint src app Examples` before pushing if hlint is installed locally.
- GHC 9.12.1 (Homebrew) works locally but is not in the CI matrix. Avoid using APIs newer than GHC 9.2; check the `tested-with` field in `MuCheck.cabal`.
- The `.ghc.environment.<arch>-<os>-<version>` file generated by `--write-ghc-environment-files=always` is required for the smoke test. Do not commit it (it is already in `.gitignore`).

## Testing posture

Tests live in `test/` and use hspec with hspec-discover. All test files must have a `Spec` suffix.

**Do not assert on hardcoded mutation counts.** Counts change whenever a mutator is added or modified. They are implementation details, not public behaviour. The commented-out tests in `test/Test/MuCheck/MutationSpec.hs` that assert on raw AST values are left as reference — do not uncomment and rely on them.

**Assert on behaviour instead:**
- Each `selectXxx` function returns a non-empty list for a module that contains the relevant construct.
- Mutated modules differ from the original after `prettyPrint`.
- `mutatesN` with order 1 produces at least as many results as `once` alone.
- The `Show` instance for `MuOp` produces output containing `==>`.
- `MAnalysisSummary` `Show` output contains `"Total mutants:"`, `"Killed:"`, and a percentage.

**After running any test that writes to `.mutants/`**, the directory will be left behind. It is safe to delete: `rm -rf .mutants/`.
