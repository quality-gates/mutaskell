# mutaskell [![CI](https://github.com/quality-gates/mutaskell/actions/workflows/mutation.yml/badge.svg)](https://github.com/quality-gates/mutaskell/actions/workflows/mutation.yml) [![Docs](https://github.com/quality-gates/mutaskell/actions/workflows/pages.yml/badge.svg)](https://quality-gates.github.io/mutaskell) [![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](LICENSE)

mutaskell is a mutation testing tool for Haskell. It tweaks your source in small ways and checks whether your tests catch the change. If they don't, that's a gap in your test suite worth closing.

mutaskell started as a fork of [MuCheck](https://github.com/vrthra/mucheck) by Duc Lee and Rahul Gopinath, rewritten to use the GHC parser and extended with a full CLI, coverage-guided mutation, parallel evaluation, and CI integrations.

# Why mutation testing?

Code coverage tells you which lines executed during a test run. It tells you nothing about whether the tests would catch a bug. A test suite that calls every function without ever checking return values will show 100% coverage while catching nothing.

Mutation testing answers the harder question: **if the code were wrong, would the tests fail?** mutaskell makes small, deliberate changes to your source — flipping `+` to `-`, swapping `True` to `False`, dropping a base case, replacing `Just x` with `Nothing` — and re-runs the test suite. If the tests still pass, that mutation *escaped*. An escaped mutant means a real bug of that shape would go undetected in production too.

## It catches AI-generated test slop

LLMs are good at producing tests that pass. They're not optimising for tests that would catch bugs — they're producing code that looks like tests. Mutation testing exposes the difference.

**Testing existence instead of value.** An LLM asked to test a lookup function will often write:

```haskell
it "finds the user" $
  Map.lookup userId db `shouldSatisfy` isJust
```

Mutate the value stored in the map — wrong role, wrong email, anything — and this test still passes. It only checks that *something* came back. `shouldBe (Just expectedUser)` kills the mutant; `shouldSatisfy isJust` does not.

**Avoiding boundary values.** Given a validation function:

```haskell
isValidEmail :: String -> Bool
isValidEmail s = '@' `elem` s && '.' `elem` s
```

An LLM writes ``isValidEmail "user@example.com" `shouldBe` True``. Mutate `&&` to `||` — the test input has both characters, so it still returns `True` and the test still passes. The mutant only dies when you test an input like `"user@example"` (has `@` but no `.`), which the LLM never thought to include.

**Testing the wrong property.** For a sort function, an LLM often asserts on length rather than order:

```haskell
it "sorts a list" $
  length (qsort [3,1,2]) `shouldBe` 3
```

Mutate the comparator so equal elements end up in the wrong partition. Length is still 3. The test passes. ``qsort [3,1,2] `shouldBe` [1,2,3]`` kills it immediately.

**Testing each function in isolation with cherry-picked inputs.** A pipeline `parse → validate → process` gets three separate unit tests, each with a hand-crafted input that happens to work. Mutate `parse` to return a slightly wrong value — a default port of `0` instead of `80` — and all three tests still pass because `validate` and `process` are fed their own canned inputs, not `parse`'s output. Only an end-to-end test that feeds a real input through the whole pipeline and checks the final result would catch it.

The common thread: the tests verify that code ran, not that it produced the right answer. Mutation testing forces that distinction into the open.

## The metric to track: covered-MSI

Raw MSI counts mutations in code your tests never execute. Add a new function with no tests yet, and your score drops — not because your existing tests got weaker, but because there's more untested code. That's a coverage problem, not a test quality problem, and conflating the two makes the number hard to act on.

Covered-MSI only counts mutations in lines your test suite actually ran through, using GHC's HPC coverage data. It stays flat when you add untested code and drops when your existing tests stop catching things they used to catch. That's the signal worth gating on in CI.

Use `--min-covered-msi 70` as a starting point. To get covered-MSI, build your test suite with `--enable-coverage` and pass the resulting `.tix` file via `--tix` — the get-started section below shows the exact commands.

## Get started

**Minimal run** (no coverage data):

```bash
cabal build --write-ghc-environment-files=always all
cabal run mutaskell -- src/YourModule.hs
```

**With coverage** (recommended — unlocks covered-MSI):

```bash
# Build and run your test suite with coverage instrumentation
echo "package your-package-name" > cabal.project.local
echo "  coverage: True" >> cabal.project.local
cabal run exe:your-test-suite
rm -f cabal.project.local

# Copy .mix files where mucheck can find them
mkdir -p .hpc
find dist-newstyle -name "*.mix" -exec cp {} .hpc/ \;

# Pass the generated .tix file to mucheck
cabal run mutaskell -- --tix your-test-suite.tix src/YourModule.hs
```

**With a config file** — drop a `.mucheck.yaml` in your project root:

```yaml
min_covered_msi: 70    # fail CI if covered-MSI drops below 70%
timeout: 30            # kill slow mutant evaluations after 30s
workers: 4             # evaluate 4 mutants in parallel
quiet: true            # only print surviving mutants, not every kill
```

**CI and config examples** — the `setups/` directory in this repo contains ready-to-use files:

| File | What it is |
| :--- | :--- |
| `setups/github-actions.yml` | Complete GitHub Actions workflow; copy to `.github/workflows/mutation.yml` |
| `setups/gitlab-ci.yml` | GitLab CI job with Code Quality artifact; paste into `.gitlab-ci.yml` |
| `setups/conservative.mucheck.yaml` | Low bar for first adoption; rename to `.mucheck.yaml` and raise the threshold over time |
| `setups/strict.mucheck.yaml` | High bar for mature or greenfield projects |
| `setups/diff-only.mucheck.yaml` | Only mutates changed lines; pair with `--git-diff-base` for fast PR checks |

The `--write-ghc-environment-files=always` flag is required so the `hint` interpreter can find your project's modules at runtime. The `.ghc.environment.*` file it generates is already in `.gitignore`.

# Documentation

Full documentation is available on the [mutaskell GitHub Pages site](https://quality-gates.github.io/mutaskell).

# Install required packages:

```
$ cabal update
$ cabal install cabal-install
$ cabal install --only-dependencies --enable-tests
```
# Use the provided sample adapter

We are going to use the simplistic `Test.Mutaskell.TestAdapter.AssertCheck`
module for our example.

First, we need the coverage information of our tests. While it is not
a required part, it is *strongly* recommended that you provide the coverage
information of your module using `-fhpc` flag to ghc. mutaskell can cut down
on the number of mutants generated drastically by using the `HPC` information.
In order to run `sample-test` with coverage enabled we have to pass the `--enable-coverage` flag to cabal.
The coverage information is written to the file`sample-test.tix` in the current directory.

```
cabal run --enable-coverage exe:sample-test
```

We are now ready to run mucheck, let us run it.

```
cabal run mutaskell -- --tix sample-test.tix Examples/AssertCheckTest.hs
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

All the steps above can also be done by running this make command in mutaskell
directory.

```
make hpcex
```

## Important

Currently `mutaskell` is restricted to running mutation analysis on a single
module at a time. In order for it to work, the module being tested should
contain the tests also.

mutaskell discovers test functions in two ways:

1. **Naming convention**: Functions whose names start with `prop_`, `test_`, or `spec_` are picked up automatically.
2. **Annotation**: Any function annotated with `{-# ANN <name> "Test" #-}` is treated as a test. Use this as an opt-in override for names that do not follow a convention.

If you have supporting functions that should not be mutated, annotate them with `{-# ANN <name> "TestSupport" #-}`.
This allows mutaskell to find the tests to run, and also to figure out which of
the functions to leave alone while mutating.

Take a look at the `Examples/AssertCheckTest.hs` to see how mucheck expects the
module to be.

## Supported Mutations

mutaskell currently supports:

1.  Literal values (Int, Float, Char, String, Bool)
2.  Standard functions and operators substitution (includes `&&`/`||` swap, `foldl`/`foldr` swap, and all comparison, arithmetic, and bitwise operator groups)
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
24. Explicit list literal emptying or one-element removal: `[x, y, z]` → `[]` or `[x, z]` etc. (`list-literal`)
25. Monadic bind stripping: `x <- action` → `_ <- action`, testing that the bound value is used (`bind-to-sequence`)
26. Pattern constructor flip: `Just x`/`Nothing`, `Left e`/`Right e`, `True`/`False` in patterns (`pattern-constructor`)
27. Append strip: `xs ++ ys` → `xs` or `ys`, testing that both halves of a concatenation are needed (`append-strip`)
28. Argument flip for known binary functions: `compare x y` → `compare y x` (`flip-args`)
29. `seq` strip: `seq x y` → `y`, testing that forced evaluation is required (`seq-strip`)
30. Tuple component swap: `(a, b)` → `(b, a)` (`tuple-swap`)
31. `Ordering` literal flip: `GT` ↔ `LT`, `EQ` → `GT` or `LT` (`ordering-literal`)

### Mutation Types: Before & After

Each mutant is a single, minimal change to your source. Below is a concrete example for every mutation type.

**1. Literal values** — substitutes nearby numeric, char, string, or bool literals

```haskell
-- Before
threshold = 10
-- After
threshold = 11
```

**2. Functions and operators** — swaps operators or functions within configured groups (arithmetic, comparison, bitwise, logical `&&`/`||`, folds `foldl`/`foldr`, and more)

```haskell
-- Before
x = a + b
-- After (arithmetic)
x = a - b
-- Before
result = a && b
-- After (logical)
result = a || b
-- Before
sorted = foldl f z xs
-- After (fold direction)
sorted = foldr f z xs
```

**3. If-else swapping** — swaps the then and else branches

```haskell
-- Before
if valid then "ok" else "fail"
-- After
if valid then "fail" else "ok"
```

**4. Guarded boolean negation** — negates a guard condition

```haskell
-- Before
f x | x > 0 = "positive"
-- After
f x | not (x > 0) = "positive"
```

**5. Pattern match permutation and removal** — reorders clauses or drops one

```haskell
-- Before
classify 0 = "zero"
classify n = "nonzero"
-- After (permutation)
classify n = "nonzero"
classify 0 = "zero"
```

**6. `not` removal** (`remove-not`) — strips `not` from a negated predicate

```haskell
-- Before
guard (not (null xs))
-- After
guard (null xs)
```

**7. `negate` removal** (`remove-negation`) — strips `negate` from an expression

```haskell
-- Before
abs (negate x)
-- After
abs x
```

**8. `case...of` alternative removal** (`case-alt-remove`) — removes one branch

```haskell
-- Before
case x of { Just v -> v; Nothing -> 0 }
-- After
case x of { Nothing -> 0 }
```

**9. Default alternative removal** (`case-default-remove`) — removes the `_` or `otherwise` branch

```haskell
-- Before
case x of { 0 -> "zero"; _ -> "other" }
-- After
case x of { 0 -> "zero" }
```

**10. Do-block statement removal** (`remove-stmt`) — removes one statement from a `do` block

```haskell
-- Before
do
  logEvent ev
  processEvent ev
-- After
do
  processEvent ev
```

**11. Let-binding removal** (`remove-let-binding`) — removes one binding from a `let...in` or `do let`

```haskell
-- Before
let result = compute x
    adjusted = result + 1
in adjusted
-- After
let adjusted = result + 1
in adjusted
```

**12. Where-binding removal** (`remove-where-binding`) — removes one binding from a `where` clause

```haskell
-- Before
f x = g y
  where y = x + 1
-- After
f x = g y
```

**13. Self-assignment removal** (`remove-self-assign`) — removes `let x = x` or `x <- return x`

```haskell
-- Before
do
  x <- return x
  process x
-- After
do
  process x
```

**14. Numeric literal negation** (`negate-literal`) — wraps a numeric literal with `negate`

```haskell
-- Before
offset = 42
-- After
offset = negate 42
```

**15. String literal replacement** (`string-literal`) — replaces the string in a comparison with `""`

```haskell
-- Before
x == "hello"
-- After
x == ""
```

**16. Boolean operand replacement** (`bool-operand`) — replaces one operand of `&&` or `||` with `True` or `False`

```haskell
-- Before
valid && authorised
-- After
True && authorised
```

**17. `Maybe` flipping** (`flip-maybe`) — swaps `Just x` and `Nothing`

```haskell
-- Before
Just result
-- After
Nothing
```

**18. `Either` flipping** (`flip-either`) — swaps `Right x` and `Left x`

```haskell
-- Before
Right result
-- After
Left result
```

**19. Concurrency wrapper removal** (`remove-forkIO`) — drops `forkIO`, `async`, or `withAsync`

```haskell
-- Before
forkIO (worker queue)
-- After
worker queue
```

**20. Resource bracket degeneration** (`bracket-degenerate`) — removes the release action from `bracket`

```haskell
-- Before
bracket acquire release action
-- After
acquire >>= action
```

**21. Exception handler removal** (`error-guard`) — replaces `catch`/`handle`/`try` with a no-op

```haskell
-- Before
catch (riskyOp x) handler
-- After
riskyOp x
```

**22. Mutable argument replacement** (`replace-mutable-arg`) — replaces an `IORef`/`MVar`/`TVar` argument with `undefined`

```haskell
-- Before
modifyIORef ref (+1)
-- After
modifyIORef undefined (+1)
```

**23. Zero-return** (`zero-return`) — replaces the body of a function clause with the zero value for its return type

```haskell
-- Before
isValid x = x > 0
-- After
isValid x = False
```

**24. Explicit list literal** (`list-literal`) — empties a non-empty list or removes one element

```haskell
-- Before
xs = [1, 2, 3]
-- After (empty)
xs = []
-- After (one removed)
xs = [2, 3]
```

**25. Monadic bind stripping** (`bind-to-sequence`) — replaces `x <- action` with `_ <- action`, testing that the bound value is used downstream

```haskell
-- Before
result <- readFile path
return result
-- After
_ <- readFile path
return result  -- compile error: 'result' unbound
```

**26. Pattern constructor flip** (`pattern-constructor`) — flips a constructor in a pattern: `Just`↔`Nothing`, `Left`↔`Right`, `True`↔`False`

```haskell
-- Before
f (Just x) = x + 1
f Nothing  = 0
-- After
f Nothing  = x + 1  -- x unbound: compile error (killed)
f (Just _) = 0
```

**27. Append strip** (`append-strip`) — replaces `xs ++ ys` with `xs` or with `ys`, testing that both halves are needed

```haskell
-- Before
result = prefix ++ suffix
-- After (left only)
result = prefix
-- After (right only)
result = suffix
```

**28. Argument flip** (`flip-args`) — swaps the two arguments of a known binary function

```haskell
-- Before
cmp = compare x y
-- After
cmp = compare y x
```

**29. `seq` strip** (`seq-strip`) — removes `seq x y`, replacing it with `y`, testing that the forced evaluation is required

```haskell
-- Before
f x acc = seq acc (acc + x)
-- After
f x acc = acc + x
```

**30. Tuple swap** (`tuple-swap`) — swaps the two components of a pair expression

```haskell
-- Before
pair = (key, value)
-- After
pair = (value, key)  -- compile error when types differ (killed)
```

**31. `Ordering` literal flip** (`ordering-literal`) — flips `GT` ↔ `LT`; replaces `EQ` with `GT` or `LT`

```haskell
-- Before
cmp = GT
-- After
cmp = LT
```

### Language Extensions

mutaskell uses the actual GHC parser (via `ghc` + `ghc-exactprint`) so it
supports **all** GHC language extensions out of the box — `LambdaCase`,
`TypeFamilies`, `GADTs`, `LinearTypes`, `OverloadedStrings`, and anything else
GHC 9.12 accepts.  There is no extension whitelist; if GHC can parse your
source, mutaskell can mutate it.

### Command-Line Interface

mutaskell supports several CLI flags for configuring mutation runs and output:

*   `--dry-run`: Show mutation counts by type without evaluating.
*   `--noop`: Verify tests pass on unmodified source first (exits with 3 on failure).
*   `--fail-on-escaped`: Exit with code 4 if any mutant survives.
*   `--min-msi PCT`: Exit with code 5 if overall MSI is below PCT percent.
*   `--min-covered-msi PCT`: Exit with code 5 if covered-code MSI is below PCT percent (requires `--tix`).
*   `--ignore-msi-with-no-mutations`: Treat MSI quality gates as passed when no mutable constructs are found.
*   `--disable NAME` / `--enable NAME`: Skip or run only mutants of the named type (repeatable).
*   `--quiet`: Suppress output for killed and errored mutants; show only alive mutants.
*   `--verbose`: Print per-mutant evaluation details (mutant source, test output).
*   `--debug`: Print raw interpreter diagnostics and mutant type during a run.
*   `--no-diffs`: Suppress the per-mutant unified diff output.
*   `--output-statuses CHARS`: Filter terminal output to specific result types (`k`, `a`, `e`, `s`).
*   `--workers N`: Evaluate mutants concurrently using N subprocess workers (default: 1). Uses process-level parallelism to keep hint evaluations isolated.
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
*   `--coverage`: Auto-discover a `.tix` coverage file in the current directory without requiring `--tix FILE`.
*   `--config FILE`: Load config from FILE instead of auto-loading `.mucheck.yaml` from the project root.

### JSON logger output

The `--logger-json FILE` flag writes a compact JSON summary after each run. The `msi` and `covered_code_msi` fields are always reported on a **0–1 scale** (not as percentages):

```json
{
  "total": 42,
  "killed": 30,
  "alive": 10,
  "skipped": 2,
  "errors": 0,
  "msi": 0.75,
  "covered_code_msi": 0.80
}
```

`covered_code_msi` is `null` when no `--tix` file is provided. The `--logger-agentic-json` output uses the same 0–1 scale for `msi` in its `summary` object.

### Config file

mutaskell auto-loads `.mucheck.yaml` from the project root if it exists. CLI flags override config values. Supported keys:

```yaml
# .mucheck.yaml — example config; unknown keys are rejected with an error
# JSON Schema: schema/mucheck-config-schema.json
min_msi: 80                       # Minimum required MSI (0–100); exit 5 if below
min_covered_msi: 80               # Minimum required covered-code MSI (requires --tix)
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


