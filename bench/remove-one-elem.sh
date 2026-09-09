#!/bin/sh
# Single-deletion removal benchmark: force direct and legacy removeOneElem
# variants over growing inputs, plus the production sampled generation path
# (cap 1) for a literal list module.  Prints elapsed time and allocation.
#
# Usage: bench/remove-one-elem.sh
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
cabal exec -- ghc -O1 -rtsopts -with-rtsopts=-T -package mutaskell \
    -odir "$WORK" -hidir "$WORK" "$ROOT/bench/remove-one-elem-bench.hs" \
    -o "$WORK/remove-one-elem-bench"

run() {
    "$WORK/remove-one-elem-bench" "$@"
}

echo "== direct single deletions =="
for size in 20 22 24 26 28; do
    run direct "$size"
done

echo "== legacy all-but-one combinations =="
for size in 20 22 24 26 28; do
    run legacy "$size"
done

echo "== production sampled generation, cap 1 =="
for size in 20 22 24 26 28; do
    run sampled "$size"
done
