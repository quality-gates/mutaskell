# Coding standards

## Tests

- Strongly prefer integration tests and end-to-end tests over unit tests.
- Strongly prefer exercising real system behaviour over "the tests pass so it must work."
- Only mock third-party services we cannot control. Do not mock code we own.
- For this codebase, the default proof is: run the real `mutaskell` / `mucheck` pipeline on example or project sources and assert kill/escape behaviour, summaries, and exit outcomes — not hardcoded mutant counts.

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
