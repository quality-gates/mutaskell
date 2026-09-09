#!/bin/sh
# Sampling benchmark: force half-sample and fixed-cap draws over growing
# inputs.  Prints elapsed time separately from allocation and residency.
#
# Usage: bench/sample.sh
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
cabal exec -- ghc -O1 -rtsopts -with-rtsopts=-T -package mutaskell \
    -odir "$WORK" -hidir "$WORK" "$ROOT/bench/sample-bench.hs" \
    -o "$WORK/sample-bench"

run() {
    "$WORK/sample-bench" "$@"
}

echo "== half-sample =="
for size in 4000 8000 16000 32000; do
    run half "$size"
done

echo "== fixed-cap 300 =="
for size in 4000 8000 16000 32000; do
    run cap "$size"
done
