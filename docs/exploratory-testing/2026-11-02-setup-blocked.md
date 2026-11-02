# Exploratory testing: CLI, project mode and baselines

- Date: 2026-11-02; starting commit: `1ccad0a010f25ad117f007a9addd4c4f69825ab5` (release 0.8.58).
- Interface: `cabal run mutaskell -- …` in a Linux worktree detached from `origin/main`. Example and synthetic project data were isolated from the main checkout.
- Evidence: [commands and output](evidence/2026-11-02/), including the [container setup](evidence/2026-11-02/container-setup.txt), [initial session](evidence/2026-11-02/session.log), [project replays](evidence/2026-11-02/replay.log), [missing-rsync reproduction](evidence/2026-11-02/replay-args.log), [replay with rsync](evidence/2026-11-02/with-rsync.log), and [bounded-heap suite](evidence/2026-11-02/full-retry.log).
- Outcome: three journeys exercised; no confirmed product bugs and no new issues. The environment and dependency-lock limitations below qualify the results.

## Setup and limitations

The first attempt had no `ghc`, `cabal`, `stack`, or `mutaskell` on the host. All six attempted commands exited 127 (`cabal: command not found`) before mutaskell launched; no baseline was created. Release 0.8.58 had no binary asset and the checked CI run had no build artifact. The repository has no Cabal freeze file, `cabal.project`, or Stack lock file; `skills-lock.json` locks agent skills, not Haskell dependencies.

For the retry, an isolated, resource-capped container used `haskell:9.12-slim-bookworm` (image digest `sha256:4e31ab38c4b87dd51fcef424a7cea074c7b4e32883e73ab4b5731706a1d9ac6b`), GHC 9.12.4 and cabal-install 3.14.1.1. The project tests GHC 9.12.1 in CI; this retry is not that compiler. After `cabal update`, `cabal freeze --enable-tests` generated a [scratch dependency snapshot](evidence/2026-11-02/cabal.project.freeze) at Hackage index state `2026-11-02T15:04:01Z`. Its SHA-256 was `4ab734e718ebb5dac41e716acc505562945331615b216c33d35cc07757bfe147`, unchanged after the runs. Builds used that snapshot, but it was generated for this pass, **not a repository-approved lockfile**; the requested project-lockfile installation therefore remained unavailable. Cabal's home, package cache, store, generated freeze file and build outputs were kept in the worktree.

The slim image initially lacked `rsync`, which parallel project mode invokes. For the investigation, Debian bookworm `rsync` 3.2.7-1+deb12u6 and `libpopt0` 1.19+dfsg-1 were extracted into the scratch worktree, not installed system-wide. Their `.deb` SHA-256s were respectively `ce99de6b36cbb62dc614a73463e467f2c1ef509545a9a2b33cab19b50530b672` and `6f94b488255acd996254f775c77ff3956557c61f860a3c9caeaf65457554194f`. The initial and altered-environment results are both retained.

## Journeys

### 1. Mutate a single example and inspect the results

**Expectation:** the documented example completes, kills mutants, and reports results; `--dry-run` counts mutations without evaluating them.

- `cabal run --enable-tests mutaskell -- --dry-run Examples/AssertCheckTest.hs --workers 1 --timeout 30` exited 0 and showed an upper bound of 68.
- The ordinary command without `--dry-run` exited 0: 68 evaluated, 63 killed, 4 alive, 1 skipped and 0 errors; the `functions` mutator killed 5. This matches the 63 kills in the [previous example run](2026-10-03-baselines-diffs-scoping-targeted-runs.md). See [session output](evidence/2026-11-02/session.log).

### 2. Discover and mutate a project

**Expectation:** project mode discovers the isolated source, builds/tests the unmodified project, evaluates a mutant, restores source and clears progress. The synthetic project contained a `cabal.project` marker and one `M1.hs`; `true` represented both its build and test commands, so this checks orchestration rather than a real build/test integration.

- With `--dry-run --workers 1 --jobs 1 --timeout 30 --time-budget 1800`, discovery found one source file and reported 4 generated mutants; exit 0.
- With `--build-cmd true --test-cmd true --max-mutants 1` and the same limits, baseline passed, one mutant survived and the command exited 0. The source's [SHA-256](evidence/2026-11-02/project-before.sha256) was unchanged and `.mutaskell/progress` was absent afterward. See [replay output](evidence/2026-11-02/replay.log).
- A separate argument-quoting variation used `--test-args` containing a space and an apostrophe. It passed with one project job. The `--jobs 2` variation initially failed because `rsync` was absent, and passed twice after adding it to the scratch PATH; see the observation below. With one source file it launched one effective job, so concurrent project jobs were not tested. These comparisons ran in a CPU- and memory-capped container.

### 3. Create and update a baseline

**Expectation:** `--update-baseline FILE` records escaped mutants, and `--baseline FILE --update-baseline FILE` preserves existing IDs while excluding those mutants from the next run.

- Creating the baseline exited 0 with 63 killed, 4 alive, 1 skipped and 0 errors. The baseline held four IDs ([file](evidence/2026-11-02/baseline-before.txt), [JSON](evidence/2026-11-02/create.json)).
- Reusing and updating that file exited 0 with 63 killed, 0 alive, 1 skipped and 0 errors among 64 mutants ([JSON](evidence/2026-11-02/update.json)). The [before](evidence/2026-11-02/baseline-before.txt) and [after](evidence/2026-11-02/baseline-after.txt) files were byte-identical. This exercises the preservation scenario from closed [#128](https://github.com/quality-gates/mutaskell/issues/128); it does not measure the separate performance issue [#142](https://github.com/quality-gates/mutaskell/issues/142).

## Observations and verification

- **Missing external utility, not a confirmed product defect:** the first full local suite had 442 examples and one failure in the project-worker argument test. Two targeted replays also failed. A direct CLI replay showed `rsync: callProcess: posix_spawnp: permission denied` on both two-job attempts, while one-job mode passed. With scratch `rsync` added, the targeted test and both two-job replays passed. The CLI exposes an uncaught exception rather than a clear missing-utility diagnostic; the pass did not establish a documented promise about that diagnostic, so no bug was filed. Closed [#112](https://github.com/quality-gates/mutaskell/issues/112) covers a different, fixed test-arguments problem.
- **Full-suite retry:** with rsync available, a subsequent full suite exited 1 without a final example summary; its cause was not established. A further full invocation with `--ghc-options=-rtsopts --test-options='+RTS -M2G -RTS'` exited 0: **442 examples, 0 failures**. Its recorded container memory events reported zero OOM kills; that does not explain the preceding run. The build, the example smoke run and `cabal haddock --enable-tests all` also exited 0. Haddock reported one undocumented symbol, `macroScanCountRef`, which already exists in unchanged `origin/main` source. It also printed link warnings; no before/after comparison was made to establish whether any were new. Details and commands are in the linked logs and scripts.
- Coverage-guided mutation, real external project builds, quality-gate failures and large-baseline performance were not explored. Successful runs on GHC 9.12.4 and a generated snapshot do not establish behaviour for every supported dependency selection.
