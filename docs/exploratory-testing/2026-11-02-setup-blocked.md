# Exploratory testing attempt: setup blocked

- Date: 2026-11-02.
- Starting commit: `1ccad0a010f25ad117f007a9addd4c4f69825ab5`, release `0.8.58`.
- Host: Linux. The attempt used a separate worktree based on `origin/main`.
- Product interface: the documented `cabal run mutaskell -- ...` CLI. No product command reached mutaskell.
- Isolation: no dependencies were installed, and no background processes, containers, images, volumes, or listeners were started.

## Setup result

`ghc`, `cabal`, `stack`, and `mutaskell` were absent from `PATH`. The repository contains no Cabal freeze file, `cabal.project`, or Stack lock file. `skills-lock.json` locks agent skills, not Haskell dependencies. The requested locked dependency installation was therefore unavailable. Release `0.8.58` has no binary assets, and the recent successful build run `40874070469` has no artifacts. A Docker daemon was available, but the checked-in Dockerfile resolves dependencies without a lock file. The attempt did not run that unlocked build.

The following commands ran in the worktree. Each failed with `/bin/bash: cabal: command not found` and exit code 127. None produced a product output or a baseline file.

```console
cabal run mutaskell -- Examples/AssertCheckTest.hs --workers 1 --timeout 30
cabal run mutaskell -- --dry-run Examples/AssertCheckTest.hs --workers 1 --timeout 30
cabal run mutaskell -- . --dry-run --workers 1 --jobs 1 --timeout 30 --time-budget 1800
cabal run mutaskell -- . --workers 1 --jobs 1 --timeout 30 --time-budget 1800
cabal run mutaskell -- Examples/AssertCheckTest.hs --update-baseline .explore/baseline --workers 1 --timeout 30
cabal run mutaskell -- Examples/AssertCheckTest.hs --baseline .explore/baseline --update-baseline .explore/baseline --workers 1 --timeout 30
```

## Intended journeys

1. **Single-file mutation.** Run the documented example and compare with a dry run. The ordinary run and variation both stopped before Cabal started. Mutant results and the effect on disk remain untested.
2. **Project discovery and evaluation.** Run project mode and its dry-run variation. Both stopped before Cabal started. Discovery, reports, and progress cleanup remain untested.
3. **Baseline lifecycle.** Create a baseline, then use it while updating it. Both attempts stopped before Cabal started. `.explore/baseline` did not exist afterward. This does not test whether baseline updates preserve IDs.

No user journey completed. Coverage-guided mutation, external test commands, quality gates, and other workflows were not attempted.

## Findings and next step

No product bugs were confirmed or rejected, and no issue was filed. The missing toolchain and missing Haskell dependency lock are setup constraints, not observed product failures. The existing [baseline performance issue #142](https://github.com/quality-gates/mutaskell/issues/142) concerns a source-level audit and is not a finding from this attempt. The prior [baseline correctness issue #128](https://github.com/quality-gates/mutaskell/issues/128) is closed and was not retested here.

To repeat this pass under the requested isolation, provide a project-approved Haskell dependency lock and a GHC 9.12.1 and Cabal toolchain, or explicitly authorize a different dependency policy. Install mutable dependencies and build outputs inside a new worktree, then run the documented build before repeating the journeys.
