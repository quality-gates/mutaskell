# Exploratory testing: mutaskell CLI journeys

- **Date**: 2026-09-10
- **Build**: branch `worker-mutant-transport` @ `831f6c2`, mutaskell 0.8.23
- **Toolchain**: GHC 9.12.1 (Homebrew), macOS aarch64 (Darwin 25.6.0), 8 CPUs
- **Setup**: `cabal build --write-ghc-environment-files=always all` (clean)
- **Evidence**: `/tmp/et-mutaskell/` (run logs, logger outputs, timings, `demo/` scratch project)
- **Interface driven**: the `mutaskell` CLI only. No internal APIs were called.

## Baseline health

The documented smoke test passes and is the reference point for every comparison below:

```
cabal run mutaskell -- Examples/AssertCheckTest.hs
→ MSI 92%, 68 mutants, 63 killed, 4 alive, 1 skipped, 0 errors
```

## Journeys exercised

### J1 — Mutate a single module and read the score

Goal: run the tool on a Haskell file and get a trustworthy mutation score.

Ordinary path passes. `--dry-run` (68 upper-bound, matches the evaluated total), `--quiet`, `--no-diffs`, `--enable`/`--disable`, and `--keep-mutants` all behave as documented. Kept mutant sources were inspected on disk and are syntactically valid; the single surviving `pattern-match` mutant is a genuine equivalent mutant (it swaps two disjoint `qsort` clauses), not a defect.

Variations: baseline round-trip (`--update-baseline` then `--baseline`) correctly suppresses the known survivor; `--ignore-msi-with-no-mutations` correctly converts the resulting zero-mutant gate failure (exit 5) into exit 0. Exit codes observed and correct: 0, 3 (baseline failure), 4 (`--fail-on-escaped`), 5 (`--min-msi` / `--min-covered-msi`).

**Confirmed bug found: [#61](https://github.com/quality-gates/mutaskell/issues/61)** — `--logger-agentic-json` emits invalid JSON.

### J2 — Gate a project in CI (project / `--exec` mode)

Goal: point mutaskell at a real cabal project and gate the build on a score.

Driven against a purpose-built minimal cabal project (`/tmp/et-mutaskell/demo`: library + exitcode-stdio test suite, both green) — a realistic consumer rather than mutaskell's own tree. Running `mutaskell .` from the project root works end to end: baseline build, baseline test, 6 mutants, 4 killed, MSI 66%, survivors written to `.mutaskell/survivors.txt` with readable diffs. Lasting effects checked: **source files are correctly restored** after the run.

`--jobs 2` was verified genuinely parallel (interleaved worker output, 28.5s for two shards) — unlike `--workers`, see J3.

**Confirmed bugs found: [#62](https://github.com/quality-gates/mutaskell/issues/62)** (documented `mutaskell src` invocation always fails the baseline build) and **[#60](https://github.com/quality-gates/mutaskell/issues/60)** (second run in the same directory evaluates 0 mutants and fails the gate).

### J3 — Speed up a run with parallel workers

Goal: `--workers 4` should finish faster than the default on an 8-CPU host, with the same result.

Correctness holds — every worker count produces byte-identical kill counts and per-mutator breakdowns. Performance does not.

**Confirmed bug found: [#59](https://github.com/quality-gates/mutaskell/issues/59)** — process-concurrency sampling never exceeds 1 child; `--workers 4` is 1.4x–2.3x *slower* than `--workers 1`. Root cause traced to the missing `-threaded` in the `executable mutaskell` cabal stanza.

### J4 — Coverage-guided run (partial)

Goal: supply a `.tix` file and gate on covered-MSI.

Only the failure modes were exercised; a real coverage-instrumented run was **not** completed (see Unexplored). `--coverage` auto-discovery degrades gracefully with a clear message.

**Confirmed bug found: [#63](https://github.com/quality-gates/mutaskell/issues/63)** — a missing `--tix` file is silently ignored and then fails `--min-covered-msi` at 0% without naming the file.

### J5 — Loggers and CI artifacts

All five loggers write output: `--logger-json`, `--logger-gitlab`, `--logger-github`, `--logger-html`, `--logger-agentic-json`. `--logger-json` and `--logger-gitlab` parse as valid JSON; `--logger-agentic-json` does not (#61). GitHub annotation format and GitLab Code Quality fingerprints/locations look correct. `--git-diff-base HEAD` correctly skips an unmodified file with a clear message.

## Confirmed findings

| # | Issue | Severity |
| :-- | :-- | :-- |
| 1 | [#59](https://github.com/quality-gates/mutaskell/issues/59) `--workers N` gives no parallelism (no `-threaded`); slower than serial | High — the flag's entire purpose |
| 2 | [#60](https://github.com/quality-gates/mutaskell/issues/60) `.mutaskell/progress` never cleared; re-run scores 0% and fails the gate | High — silent CI red on a warm workspace |
| 3 | [#61](https://github.com/quality-gates/mutaskell/issues/61) `--logger-agentic-json` writes unparseable JSON (`\8212`), plus double-escaped `context` | High — output is unusable by its stated consumer |
| 4 | [#62](https://github.com/quality-gates/mutaskell/issues/62) Project mode builds in the target dir, so README's `mutaskell src` always fails | Medium — documented invocation is broken |
| 5 | [#63](https://github.com/quality-gates/mutaskell/issues/63) Missing `--tix` silently ignored, then covered-MSI gate fails at 0% | Medium — misdiagnosable CI failure |

All five reproduce deterministically and were replayed from a clean starting state. Root causes were traced to specific source locations and included in each issue.

## Rejected candidates

- **`pattern-match` mutant looked like it moved a clause to the end of the module.** Rejected: the rendered unified diff is badly aligned for whole-block permutations, but the mutant file written by `--keep-mutants` is correct and contiguous. Usability issue only (below).
- **Surviving mutants in `Examples/AssertCheckTest.hs`.** Rejected: the example tests are deliberately non-exhaustive; documented as expected in `CLAUDE.md`.
- **`Examples` directory failing project mode.** Not a separate bug — same root cause as #62.

## Unresolved

None. Every candidate raised during the pass was classified.

## Unexplored / blocked

- **A real coverage-guided run.** No `--enable-coverage` build with `.mix`/`.tix` files was produced, so covered-MSI was only exercised through its failure paths. Closed issue #24 concerns a covered-MSI discrepancy between the terminal summary and `--logger-json`; that remains unverified here.
- **`--timeout`, `--timeout-coefficient`, `--noop`, `--blacklist`, `--output-statuses`, `--test-args`, `--time-budget`, `--max-mutants` under a real cap, `--git-diff-lines`.** Not driven.
- **`.mucheck.yaml` config-file loading and CLI-vs-config precedence.** Not driven.
- **CPP-heavy or multi-package projects.** Not driven.

## Usability observations

Observations, distinct from the suggested improvements that follow them.

1. **Whole-block diffs are near-unreadable.** A `pattern-match` mutant that swaps two adjacent clauses renders as a 26-line diff spanning the rest of the file, with every subsequent line shown as changed. *Suggestion: anchor the diff to the mutated declaration, or emit a minimal edit script.*
2. **Zero-mutant summaries print `(0/0)` where every other run prints a percentage** (`Errors: 0 (0/0)`, `Killed: 0/0 (0/0)`). Noted inside #60.
3. **`--jobs N` output interleaves confusingly.** Each shard prints its own full `==== Project mutation summary ====`, and the merged parallel summary is printed *before* the per-file mutant result lines. A reader sees three different MSI figures (0%, 66%, 33%) in sequence. *Suggestion: suppress per-shard summaries, or label them by shard.*
4. **The baseline-build failure message is genuinely good** — it names the command, quotes the output tail, points at `.mutaskell/exec.log`, and suggests `--build-cmd`. Its only gap is that it never mentions the working directory, which was the actual cause in #62.

## Limitations

- Single platform (macOS aarch64, GHC 9.12.1), which `CLAUDE.md` notes is not the CI matrix.
- Timings were taken on a developer machine without isolating background load; the `--workers` comparison was therefore repeated across two independent pairs, both showing the same direction, and corroborated by direct process-concurrency sampling rather than resting on wall-clock alone.
- No instrumentation or configuration changes were made to the product to obtain any finding. All observations come from the ordinary user setup.

## Cleanup

No scratch state remains inside the repository (`git status` shows only this report plus the `app/App/Worker.hi` / `Worker.o` and `performance-audit.md` files that were already untracked at session start; `.mutants/` and `.mutaskell/` are absent).

The evidence directory `/tmp/et-mutaskell/` is preserved, including the minimal scratch cabal project `/tmp/et-mutaskell/demo` — it is the reproducer for [#60](https://github.com/quality-gates/mutaskell/issues/60) and [#62](https://github.com/quality-gates/mutaskell/issues/62) and is kept deliberately rather than deleted. Note that `/tmp` is not durable across reboots; the issues themselves carry self-contained replay steps.
