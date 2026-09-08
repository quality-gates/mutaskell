#!/bin/sh
# Filter benchmark: wall time of index construction, empty-filter overhead,
# and populated filtering over growing candidate (K) and lookup sizes.
#
# Usage: bench/filter.sh
#
# Compile against a tree built with
#   cabal build --write-ghc-environment-files=always all
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null

ghc -O1 -rtsopts -iapp -outputdir "$WORK/obj" \
    -package yaml -package aeson -package optparse-applicative \
    "$ROOT/bench/filter-bench.hs" -o "$WORK/filter-bench"

run() {
    "$WORK/filter-bench" "$@"
}

echo "== index construction =="
for n in 2000 8000 32000; do
    run index-ids 0 "$n"
    run index-anns 0 "$n"
    run index-lines 0 "$n"
    run index-src 0 "$n"
done

echo "== hash cache construction =="
for k in 2000 8000 32000; do
    run cache-hash "$k" 1000
done

echo "== empty-filter overhead =="
for k in 2000 8000 32000; do
    run empty-ann "$k" 1000
    run empty-ignore "$k" 1000
    run empty-id "$k" 1000
    run empty-base "$k" 1000
done

echo "== populated filters =="
for k in 2000 8000 32000; do
    run filter-ann "$k" 1000
    run filter-ignore "$k" 1000
    run filter-id "$k" 1000
    run filter-ids "$k" 1000
done
