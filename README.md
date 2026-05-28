[![Mutation Analysis](https://github.com/jonbaldie/mucheck/actions/workflows/mutation.yml/badge.svg)](https://github.com/jonbaldie/mucheck/actions/workflows/mutation.yml)
[![HLint](https://github.com/jonbaldie/mucheck/actions/workflows/hlint.yml/badge.svg)](https://github.com/jonbaldie/mucheck/actions/workflows/hlint.yml)
[![OSV-Scanner](https://github.com/jonbaldie/mucheck/actions/workflows/osv-scanner.yml/badge.svg)](https://github.com/jonbaldie/mucheck/actions/workflows/osv-scanner.yml)
[![Docs](https://github.com/jonbaldie/mucheck/actions/workflows/pages.yml/badge.svg)](https://jonbaldie.github.io/mucheck)
[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](LICENSE)

# Documentation

Full documentation is available on the [MuCheck GitHub Pages site](https://jonbaldie.github.io/mucheck).

# Install required packages:

```
$ cabal update
$ cabal install cabal-install
$ cabal install --only-dependencies --enable-tests
```
# Use the provided sample adapter

We are going to use the simplistic `Test.MuCheck.TestAdapter.AssertCheck`
module for our example.

First, we need the coverage information of our tests. While it is not
a required part, it is *strongly* recommended that you provide the coverage
information of your module using `-fhpc` flag to ghc. MuCheck can cut down
on the number of mutants generated drastically by using the `HPC` information.
In order to run `sample-test` with coverage enabled we have to pass the `--enable-coverage` flag to cabal.
The coverage information is written to the file`sample-test.tix` in the current directory.

```
cabal run --enable-coverage exe:sample-test
```

We are now ready to run mucheck, let us run it.

```
cabal run mucheck -- -tix sample-test.tix Examples/AssertCheckTest.hs
```

This results (after a sufficiently large time) in

```
Total mutants: 19 (basis for %)
        Covered: 13
        Sampled: 13
        Errors: 0  (0%)
        Alive: 1/19
        Killed: 12/19 (63%)
```
This suggests that initially `19` mutants were generated, which was reduced to
just 13 mutants that contained mutations where test suites can find them.

The run resulted in just one of the mutants being alive, with a mutation score
of 63%.

All the steps above can also be done by running this make command in MuCheck
directory.

```
make hpcex
```

## Important

Currently `MuCheck` is restricted to running mutation analysis on a single
module at a time. In order for it to work, the module being tested should
contain the tests also. Further the tests should be annotated with
```
{-# ANN <function name> "Test" #-}
```
If you have supporting functions, they should be annotated with "TestSupport".
This allows MuCheck to find the tests to run, and also to figure out which of
the functions to leave alone while mutating.

Take a look at the `Examples/AssertCheckTest.hs` to see how mucheck expects the
module to be.

## Supported Mutations

MuCheck currently supports:

1.  Literal values (Int, Float, Char, String, Bool)
2.  Standard functions and operators substitution
3.  If-else swapping
4.  Guarded boolean negation
5.  Pattern match permutation and removal
6.  `not` removal from negated expressions (`remove-not`)
7.  `negate` removal from expressions (`remove-negation`)
8.  `case...of` alternative removal (`case-alt-remove`)
9.  Default alternative (`_` or `otherwise`) removal from `case...of` and guards (`case-default-remove`)
10. Do-block statement removal (`remove-stmt`)
11. Let-binding removal from `let...in` and `do` blocks (`remove-let-binding`)
12. Where-binding removal from declarations (`remove-where-binding`)
13. Self-assignment removal: `let x = x` and `x <- return x` (`remove-self-assign`)
14. Numeric literal negation: `42` → `negate 42` (`negate-literal`)
15. String literal replacement in comparisons with `""` (`string-literal`)
16. Boolean operand replacement in `&&` and `||` with `True`/`False` (`bool-operand`)
17. `Maybe` value flipping: `Just x` ↔ `Nothing` (`flip-maybe`)
18. `Either` value flipping: `Right x` ↔ `Left x` (`flip-either`)
19. Concurrency wrapper removal: `forkIO`, `async`, `withAsync` (`remove-forkIO`)
20. Resource bracket degeneration: `bracket acquire release action` → `acquire >>= action` (`bracket-degenerate`)
21. Exception handler removal: `catch`, `handle`, `try` replaced with no-ops (`error-guard`)
22. Mutable argument replacement: `IORef`/`MVar`/`TVar` replaced with `undefined` (`replace-mutable-arg`)
23. Zero-return: replace each function match body with the zero value for its declared return type — `False`, `0`, `""`, `Nothing`, `[]`, or `return undefined` for IO (`zero-return`)

### Language Extensions

MuCheck enables several language extensions by default to ensure it can parse modern Haskell code:

*   `ScopedTypeVariables`
*   `MultiParamTypeClasses`
*   `FunctionalDependencies`
*   `FlexibleInstances`
*   `FlexibleContexts`
*   `TypeFamilies`
*   `GADTs`

### Command-Line Interface

MuCheck supports several CLI flags for configuring mutation runs and output:

*   `--dry-run`: Show mutation counts by type without evaluating.
*   `--noop`: Verify tests pass on unmodified source first (exits with 3 on failure).
*   `--fail-on-escaped`: Exit with code 4 if any mutant survives.
*   `--min-msi PCT`: Exit with code 5 if overall MSI is below PCT percent.
*   `--min-covered-msi PCT`: Exit with code 5 if covered-code MSI is below PCT percent (requires `-tix`).
*   `--ignore-msi-with-no-mutations`: Treat MSI quality gates as passed when no mutable constructs are found.
*   `--disable NAME` / `--enable NAME`: Skip or run only mutants of the named type (repeatable).
*   `--quiet`: Suppress output for killed and errored mutants; show only alive mutants.
*   `--verbose`: Print per-mutant evaluation details (mutant source, test output).
*   `--debug`: Print raw interpreter diagnostics and mutant type during a run.
*   `--no-diffs`: Suppress the per-mutant unified diff output.
*   `--output-statuses CHARS`: Filter terminal output to specific result types (`k`, `a`, `e`, `s`).
*   `--timeout N`: Kill mutant evaluation after N seconds.
*   `--timeout-coefficient N`: Set per-mutant timeout to N × measured baseline test-suite runtime.
*   `--logger-github FILE`: Write GitHub Actions `::warning` annotations for escaped mutants to FILE.
*   `--logger-gitlab FILE`: Write a GitLab Code Quality JSON artifact for escaped mutants to FILE.
*   `--git-diff-base REF`: Skip mutation if the source file is not in `git diff --name-only REF`.
*   `--git-diff-lines`: Restrict mutants to lines changed relative to `--git-diff-base` (requires `--git-diff-base`).
*   `--keep-mutants DIR`: Write mutant files to DIR and keep them after evaluation (default: system temp, deleted after each evaluation).
*   `--logger-agentic-json FILE`: Write per-mutant JSON with stable IDs, descriptions, context, and MSI summary for LLM consumption.
*   `--logger-html FILE`: Write a standalone HTML mutation report to FILE with per-mutant diffs, source context, and a colour-coded summary.
*   `--test-args ARG`: Pass ARG to the test runner on every invocation (repeatable).
*   `--coverage`: Auto-discover a `.tix` coverage file in the current directory without requiring `-tix FILE`.
*   `--config FILE`: Load config from FILE instead of auto-loading `.mucheck.yaml` from the project root.

### Config file

MuCheck auto-loads `.mucheck.yaml` from the project root if it exists. CLI flags override config values. Supported keys:

```yaml
# .mucheck.yaml — example config; unknown keys are rejected with an error
# JSON Schema: schema/mucheck-config-schema.json
min_msi: 80                       # Minimum required MSI (0–100); exit 5 if below
min_covered_msi: 80               # Minimum required covered-code MSI (requires -tix)
timeout: 30                       # Per-mutant timeout in seconds
max_mutants: 200                  # Cap the total number of sampled mutants
quiet: true                       # Suppress killed/error output; show only survivors
silent_mode: true                 # Print only the final summary line; no per-mutant output
skip_without_test: true           # Skip files with no test annotations; exit 0
json_output: mucheck.json         # Always write a JSON summary here after every run
html_output: mucheck.html         # Always write an HTML report here after every run
disable_mutators: [literal-values, negate-if-else]  # Mutators to skip
enable_mutators: [functions]                         # Restrict to these mutators only
ignore_source_lines: [NOTEST, uncovered]             # Skip mutations on lines containing these substrings
exclude_dirs: [vendor/, generated/]                  # Skip target if path starts with any listed prefix
```


