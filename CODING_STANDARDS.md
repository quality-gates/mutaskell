# Coding standards

## Tests

- Strongly prefer integration tests and end-to-end tests over unit tests.
- Strongly prefer exercising real system behaviour over "the tests pass so it must work."
- Only mock third-party services we cannot control. Do not mock code we own.
- For this codebase, the default proof is: run the real `mutaskell` / `mucheck` pipeline on example or project sources and assert kill/escape behaviour, summaries, and exit outcomes — not hardcoded mutant counts.

### Production mutation gate

Changes to production Haskell in `src/` and `app/` require a mutation score of
at least 80% for the changed scope before merge. Prefer covered-MSI from the real
test suite's HPC data; use raw MSI when project mode cannot report covered-MSI
reliably. The example
smoke test proves the tool runs; it does not satisfy this production gate.

1. Record the candidate revision and mutation scope with the review. Use
   `bash scripts/dogfood.sh` for a full-project audit. It builds a private
   copy, mutates all production source files with the default mutator set and
   per-file sampling, and runs the real Cabal build and `spec` suite.
2. Require exit zero, a completed source manifest, and a score of at least 80%.
   Keep the emitted evidence directory with the review. Budget exhaustion,
   skipped source files, a failed baseline, or missing results leave the gate
   unmet, regardless of any partial score.
3. Strengthen behavioural assertions for survivors, then rerun. Keep the declared
   production scope and full test suite. A changed-scope score proves only that
   scope; report full-project scores separately. Lower thresholds, reduced
   samples, ignored empty runs, or broad suppressions cannot satisfy the gate.
   A suppression needs a specific equivalent mutation and a reviewable reason.
4. Keep completed mutation evidence with production changes. A run stopped by
   shared-host limits is incomplete evidence. Changes confined to scripts,
   documentation, or CI do not require a new production score.

The full-project runner is manual. Automated PR enforcement is deferred to the
diff-aware dogfooding workflow
([#91](https://github.com/quality-gates/mutaskell/issues/91)); existing example CI checks do not enforce
this production standard.

The script uses raw MSI because project summaries currently discard covered-MSI
metadata. Move to covered-MSI only with fresh coverage from the same revision,
verified module coverage, and failure on missing coverage. Preserve the 80%
floor and completion checks when changing metrics.

## Comments and docs

- Code comments use ASD-STE100 Simplified Technical English.
- Ground terms in `CONTEXT.md` domain language when that file exists. Do not invent synonyms for glossary terms.
- Do not write comments that only repeat what the code already makes clear.
- Do not put brittle references in README or comments (versions, line numbers, temporary paths, "as of today" claims) when those details are allowed to change.

## Common footguns

- Tautological tests (asserting the mock was called the way the test just configured it).
- Mocks of modules/services we own.
- "Green suite" treated as proof the product works for a user.
- Narrating comments and README drift magnets.
- Cheating complexity or quality gates with denser syntax, hidden branching, or indirection that does not reduce real complexity.
- Asserting exact mutant counts that churn when mutators change.
- Building without `--write-ghc-environment-files=always` then wondering why `IntegrationSpec` / hint cannot resolve library modules.

## Haskell

- GHC window follows `mutaskell.cabal` (`tested-with`, `ghc` bounds). CI verifies GHC 9.12.1; do not rely on newer APIs without bumping tested compilers deliberately.
- Build/test with Cabal. For integration and smoke paths that use `hint`, build with `--write-ghc-environment-files=always`. Do not commit generated `.ghc.environment.*` files.
- Keep `-Wall` clean on library and executable code. Run HLint against `src app Examples` per `.hlint.yaml` before merge when you touch those trees.
- Prefer pure transformation in `Test.Mutaskell.*`; isolate IO at orchestration (`app/`, interpreter, process workers).
- The `hint` interpreter is **not thread-safe**. Parallel mutant evaluation must use forked subprocesses, not in-process `forkIO` / `async` sharing one interpreter.
- Do not assert on hardcoded mutation counts. Assert behaviour: non-empty selections for relevant constructs, pretty-printed mutants differ from originals, summary `Show` contains expected labels, operator `Show` contains `==>`.
- Tests use hspec + hspec-discover under `test/`; files need a `Spec` suffix.
- Smoke via `cabal run mutaskell -- Examples/AssertCheckTest.hs` after a proper environment-file build; confirm the run completes and kill counts do not regress without cause.
- Clean leftover `.mutants/` directories after runs that write them.
- Version bumps land in the same PR as the change (`version:` and `source-repository this` tag in `mutaskell.cabal`); do not silently revert version on follow-up branches.
