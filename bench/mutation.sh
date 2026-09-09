#!/bin/sh
# Selector/rendering benchmark for issue #39.  The selector phase forces all
# pre-sampling identity checks; the sampled phase also applies and renders the
# configured cap of one mutant.  Run both fixture families separately so a
# typed-declaration lookup change is not confused with nested-subtree printing.
#
# The `phases` fixture (issue #34) profiles the generation pipeline stage by
# stage — selection, operator sampling, AST application, mutator/span
# deduplication, rendering and rendered-source deduplication — on the exact
# path and on a cap-1 sampled path over growing literal-dense modules.
#
# Build with the normal environment-file workflow, then compare the selector
# and sampled rows across sizes.  The phases are reported separately so this
# benchmark does not attribute an end-to-end speedup to selector metadata
# without a separate printing profile.
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
cabal exec -- ghc -O1 -rtsopts -package mutaskell \
    -odir "$WORK" -hidir "$WORK" "$ROOT/bench/mutation-bench.hs" \
    -o "$WORK/mutation-bench"

run() {
    (
        cd "$WORK"
        "$WORK/mutation-bench" "$@" ${MUTATION_BENCH_RTS_OPTS:-}
    )
}

echo "== typed declaration metadata, cap 1 =="
for size in 32 64 128; do
    run typed "$size"
done

echo "== nested subtrees, cap 1 =="
for size in 4 6 8; do
    run nested "$size"
done

echo "== generation phase profile, exact vs cap-1 sampled =="
for size in 25 50 100; do
    run phases "$size"
done
