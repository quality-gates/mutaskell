# Changelog

## [0.4.4] (Jonathan Baldie)
  * Add `case-alt-remove` mutator: remove one alternative at a time from `case...of` expressions
  * Add `case-default-remove` mutator: remove the catch-all `_` or `otherwise` alternative from `case...of` and guards
  * Add `remove-stmt` mutator: remove one statement at a time from `do`-blocks
  * Add `remove-let-binding` mutator: remove individual bindings from `let...in` and `do`-block `let` statements
  * Add `remove-where-binding` mutator: remove individual bindings from `where` clauses
  * Fix AST traversal to reach bindings in nested constructs
  * Add `Stmt`, `Alt`, and `Rhs` support to the `MuOp` framework

## [0.4.3] (Jonathan Baldie)
  * Add `remove-not` mutator: strip `not` from negated sub-expressions
  * Add `remove-negation` mutator: strip `negate` and prefix `-` from expressions
  * Deduplicate identical mutants (same prettyPrint output) before evaluation to avoid redundant test runs
  * Add `--disable NAME` and `--enable NAME` flags for selective mutation; trailing `*` wildcard supported
  * Add hspec tests for `selectRemoveNotOps` and `selectRemoveNegationOps`

## [0.4.2] (Jonathan Baldie)
  * Add `--fail-on-escaped` flag: exit with code 4 if any mutant survives all tests
  * Add `--min-msi PCT` flag: exit with code 5 if MSI falls below the given percentage threshold
  * Add `--noop` flag: run tests on unmodified source before mutation begins; exit with code 3 if they fail
  * Define and document exit codes: 0=pass, 2=bad arguments, 3=noop failure, 4=escaped mutants, 5=MSI below threshold; unknown flags now exit 2 instead of crashing
  * Print per-mutator breakdown table (killed / alive / errors per MuVar variant) after every run summary

## [0.4.1] (Jonathan Baldie)
  * Update `.cabal` metadata: homepage, maintainer, and source-repository stanzas to point to the jonbaldie/mucheck fork
  * Print MSI (killed ÷ (killed + alive)) as a percentage as the top-line metric in the final summary
  * Count file-write errors during mutant creation in `_maErrors`; never silently discard a mutant that could not be written to disk
  * Skip mutations that produce an AST identical to the original after `prettyPrint` to eliminate false no-op escapes
  * Add `--dry-run` flag: print a per-mutator count of all mutations that would be generated without evaluating any

## [0.4.0.1] (Rahul Gopinath)
  * Make mucheck compilable with latest Haskell libraries

## [0.4.0.0] (Rahul Gopinath)
  * Fix function mutation by making it similar to other mutation operators.
  * Allow users to specify their own groups of interchangeable functions
  * Use test annotations to get tets
  * Make testing module based.
  * Use a better datatype so that we can infer the summarization function to use.
  * Samples on mutants rather than mutation operators
  * Extracts the location of mutation for later use.
  * Add more test cases for mutations
  * Read the HPC Tix files, and use them to filter out unused mutants.
  * Failfast when we fail not when we err.
  * Add a sample adapter

## [0.3.0.0] (Rahul Gopinath)
  * Heavy refacoring, now summary is based on mutations rather than tests.
  * Mutation fails fast on either load errors or test failures

## [0.2.1.1] (Rahul Gopinath)
  * Make it usable from d-mucheck
  * Include logfile name for captured output (or the log string otherwise).

## [0.2.1.0] (Rahul Gopinath)
  * Remove need to pass in module name
  * Add new literal mutators including booleans
  * Refactor Mutation so that IO and Rand are pushed further out, and a sampler is introduced that user can override.
  * Fix the bug where maxMutants were just the first n mutants, rather than n being randomly sampled.
  * Move writefile to front where more choices about it can be made.
  * We no longer require modulename to be passed in.
  * Fix bug in replaceFst

## [0.2.0.0] (Rahul Gopinath)
  * Better documentation
  * Add tests for SYB once
  * Split it to a library and adapter modules
  * Simplify adapter interface
  * Capture stdout and stderr of test runs.

## [0.1.3.0] (Rahul Gopinath)
  * Migrate mucheck to Test namespace
  * Add more tests
  * Fix eamples in docs

## [0.1.2.2] (Rahul Gopinath)
  * Handle single pattern function definitions correctly.

## [0.1.2.1] (Rahul Gopinath)
  * Fix minor tagging issue

## [0.1.2.0] (Rahul Gopinath)
  * Update usage docs
  * Add hspec test framework support
  * Add docs on some functions
  * Refactor summarizable to require only two functions
  * Changes to allow random sampling of mutant categories
  * Refactor more functions to common utils
  * Add a changelog
  * Add a todo

## [0.1.1.0] (Rahul Gopinath)
  * Add command line args
  * Add support for executable in cabal
  * Update docs
  * Add hspec tests for common utils
  * Refactor common utils
  * Better description in cabal
  * Better summarizable class for adding new test frameworks

## [0.1.0.1] (Rahul Gopinath)
  * Refactor the entire project
  * Add tests
  * Add docs
  * Fix null check in ops
  * Add support for functions with no parameter and operators (Onetwogoo)

## [0.1.0.0]
  * Added main, examples, mucheck.cabal,readme (Rahul Gopinath)
  * Added HUnit support (Duc Lee)

## [0.0.1.0] 2013-10-11
  * Initial implementation (Duc Lee)
