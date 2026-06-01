# Orchestrator mode: status and roadmap

The goal: run mutaskell on large public Haskell repos (Pandoc, aeson, lens,
megaparsec, …) *and* on mutaskell itself, the way Infection (PHP) or a
folder-level Go tool runs — point it at real code and let the project's own
build and test suite do the work.

## Acceptance-criteria status (2026-06)

A 16-point acceptance list was agreed (DX 1–9, performance 10–16). Honest state:

| AC | What | Status | Evidence |
| :- | :--- | :----- | :------- |
| 1 | One command on a dir, exit 0 | **done** | demo + megaparsec full runs exit 0 |
| 2 | Auto-detect build/test | **done** | cabal/stack detection; runs show derived cmds |
| 3 | Walks the package | **done** | megaparsec discovered 42 files; pandoc walk |
| 4 | Survives bad files | **done** | pandoc dry-run logs `SKIP` and continues |
| 5 | Project-level score | **done** | aggregate summary printed at end |
| 6 | Tree restored | **done** | git clean after runs (only `.mutaskell/` left, gitignored) |
| 7 | Generalises to ≥2 repos | **done** | megaparsec + self-host (mutaskell) |
| 8 | Self-host | **done** | `mutaskell .` on this repo |
| 9 | Resumable + report | **done** | `.mutaskell/progress` skips done files; `.mutaskell/survivors.txt` |
| 10 | Baseline once | **done** | baseline build+test run once at startup |
| 12 | Coverage-gated generation | **done (mechanism)** | demo Calc.hs 12→11 via `.tix`; magnitude scales with uncovered fraction; cabal HPC-path auto-discovery still manual |
| 13 | Generation bounded | **done** | n! clause perms → adjacent swaps; operators sampled before rendering; 5s/file soft budget. pandoc Shared.hs ~139s → ~5s |
| 15 | Hard budget | **done** | `--max-mutants` / `--time-budget`; megaparsec "Budget exhausted; stopping" |
| 11 | No cold cabal per mutant | **NOT done** | see below |
| 14 | Parallel `--jobs N` | **done (mechanism); threshold not proven** | worker subprocesses on isolated copies, results merged; ~1.5× @ N=2 on demo. ≥2×@4 needs a balanced many-file workload |
| 16 | Throughput floor on built Pandoc | **BLOCKED** | pandoc does not solve deps on GHC 9.12.1 here; floor pending AC 11 |

### The three remaining items, honestly

**AC 11 — persistent compile/test session.** Today each mutant runs a fresh
`cabal build` then `cabal test`. The build is incremental (cabal only recompiles
the changed module), so the per-mutant cost is cabal startup + one module's
recompile + relink + the test-suite run. The real win is a persistent
`cabal repl`/GHCi session that `:reload`s only the mutated module in memory and
runs the test expression there, paying startup once. The hard part — and the
reason it is not done — is running the project's *own* test suite inside that
session generically (hspec via `hspec spec` is easy; arbitrary `exitcode-stdio`
suites are not) **without losing the build-fail→SKIPPED classification** that a
real `cabal build` gives us (a GHCi reload error is not the same signal). This is
a genuine design fork, not a small patch.

**AC 14 — parallel `--jobs N`.** Implemented: the master shards discovered files
across N isolated rsync'd copies (minus `.git`/`dist-newstyle`/`.mutaskell`), runs
one worker subprocess per shard (`--jobs 1 --only-files … --result-out …`), and
merges the tallies + survivor reports into one project score. Isolation is
mandatory because the orchestrator edits files in place. Verified: ~1.5× at N=2
on the demo with a correct merged summary. What is **not** proven is the
"4 jobs ≥2× faster" threshold — the demo has only two uneven files (so it can't
shard four ways or balance), and each worker pays a cold first build because
`dist-newstyle` is not copied. Closing it cleanly needs (a) sharding balanced by
estimated mutant count (a cheap dry-count pre-pass) and (b) a warm worker build
(relocatable `dist-newstyle` or a shared store), measured on a balanced
many-file repo.

**AC 16 — stated throughput floor.** Wanted on a built Pandoc, but Pandoc's
dependency set does not solve on GHC 9.12.1 in this environment
(`pandoc`/`commonmark-pandoc` goals unsatisfiable), so there is no built Pandoc
to measure. The honest substitute is to publish a reproducible mutants/min on a
built, green repo (megaparsec) on a named machine — but that number is dominated
by per-mutant build+test until AC 11 lands, so it should be quoted *after* the
persistent-session work, not before.

This document records what is done, what is measured, and what is left. The
numbers below come from a sweep of five shallow-cloned repos (pandoc, aeson,
megaparsec, scotty, lens). They are evidence, not vibes; re-run before trusting.

## What works now (`--exec`)

The architectural pivot is in place and proven. Instead of loading mutants into
the in-process `hint` interpreter (which cannot see a real project's deps,
config, or test suite), `--exec` writes each mutant to the source file in place
and runs the project's own `--build-cmd` / `--test-cmd`, classifying by what the
real toolchain does (build-fail → skipped, test-fail → killed, pass → alive,
timeout → killed).

Verified end to end:

* A base-only demo cabal project: 12 mutants, 7 killed / 5 alive, original
  restored intact, and a genuine test gap (an untested `>` boundary) surfaced as
  a surviving mutant.
* **mutaskell on itself** (`src/Test/Mutaskell/Utils/Common.hs`, tested via its
  own `cabal test spec`): 8 mutants, 3 killed / 2 alive / 3 skipped. The 3
  skipped were mutants the real compiler rejected (`flip-args`,
  `remove-where-binding`, `bind-to-sequence`) — correctly *not* counted as kills.

Safety invariants built in: original restored after every mutant and in
`finally`; generation forced under a timeout; output redirected to a log file
(no undrained-pipe deadlock).

## Measured blockers for "run on Pandoc easily"

A `--dry-run`-style generation sweep over the five repos (1-in-4 sample, 124
files, 5 s generation cap each) found that only ~42% of real files complete even
mutant *generation* in time:

| Outcome (sampled)                 | Share |
| :-------------------------------- | ----: |
| parsed + generated within 5 s     | ~42%  |
| parse error                       | ~16%  |
| generation exceeded 5 s (blow-up) | ~23%  |

### 1. Parse coverage = CPP — DONE (partial), verified

Every parse failure in the sample (20 of 20) was a file using CPP
(`#if`/`#ifdef`/`#include` or `{-# LANGUAGE CPP #-}`). Zero were
extension-only — `ghc-exactprint` already honours in-file `LANGUAGE` pragmas, so
injecting cabal `default-extensions` would buy nothing.

**Implemented:** `getASTFromFile` (in `Test.Mutaskell.Mutation`) parses CPP files
via `parseModuleWithCpp`, auto-discovering `cabal_macros.h` under `dist-newstyle`
and force-including it (`cppFile`) for `MIN_VERSION_*` guards. Used by `--exec`
and `--dry-run`. Verified build-free on the Pandoc clone:
`Class/CommonState.hs` went from a parse error to 8 generated mutants.

**Still open (honest edges):**
* Of the sampled CPP failures, 14/20 use `MIN_VERSION_*` and therefore need a
  built `dist-newstyle`; only 6/20 (simple `__GLASGOW_HASKELL__`/OS guards) parse
  without it. So full coverage on an *unbuilt* checkout is not achievable — but
  `--exec` requires a build anyway.
* Multi-package repos (pandoc, pandoc-cli, …) produce several `cabal_macros.h`
  with *different* package version macros. We currently force-include the first
  20 found; each header `#ifndef`-guards its macros so this is safe, but the
  "right" macros for the file's own package are not specifically selected. If a
  file mis-resolves a version guard, this is why.
* Re-run the parse sweep against a *built* repo to quantify the remaining
  failure rate.

### 2. Generation blow-up (~23% of files)

Generation is CPU-super-linear on files dense in literals (symbol tables, large
list/tuple literals). Example: `pandoc/src/Text/Pandoc/Shared.hs` burned ~30 s
of CPU in generation alone (startup is ~0.18 s, so this is the generator, not
I/O). A 93-line file (`ODT/Namespaces.hs`) timed out while a 614-line file
(`scotty/Web/Scotty.hs`) finished with 8 mutants — it tracks literal density,
not line count.

Currently guarded: `--exec` forces generation under a 90 s timeout and aborts
with an actionable message rather than hanging.

**Fix options:** (a) cap per-construct mutant counts for large literals
(e.g. don't emit O(n) one-element-removed variants for a 500-element table);
(b) profile `exactPrint` cost per mutant (re-rendering the whole module per
mutant may be quadratic); (c) coverage-gate generation so only covered
constructs are mutated.

### 3. Speed: per-mutant rebuild

Each mutant triggers a real recompile of the changed module + downstream +
relink, then a test run. Tolerable on a small leaf module; brutal on Pandoc
(minutes × hundreds of mutants). This is the dominant cost and the reason the
original tool took the interpreter shortcut.

**Fix options, roughly in order of payoff:**
* keep a persistent GHC/GHCi session and reload only the mutated module
  (pay startup once) instead of a fresh `cabal` invocation per mutant;
* coverage-guided mutation (only mutate code the tests cover) to cut mutant
  count drastically;
* parallel evaluation across worker processes (the existing `--workers`
  machinery is hint-specific; an orchestrator equivalent would shard mutants).

### 4. Whole-repo ergonomics

`--exec` takes one file. "Run on Pandoc" means a tree. Needs a directory mode
that walks `hs-source-dirs`, skips unparseable/timeout files gracefully, and
aggregates a project-level score.

## Smaller follow-ups

* MSI denominator currently includes skipped mutants (3/8 in the self-run).
  Conventionally MSI = killed / (killed + alive). This is the pre-existing
  tool-wide convention, not introduced by `--exec`; decide whether to change it
  globally.
* `--max-mutants` is config-only (`max_mutants:` in `.mucheck.yaml`); there is
  no CLI flag. A `--max-mutants N` flag would make scoping `--exec` runs easier.
