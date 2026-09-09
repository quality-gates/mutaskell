#!/bin/sh
# Positional unified-diff benchmark for issue #35.  The single-change sweep
# exercises the long unchanged prefix; the many-change sweep exercises ordered
# context interval merging and hunk rendering.
#
# Usage: bench/diff.sh
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
cabal exec -- ghc -O1 -rtsopts -with-rtsopts=-T -iapp \
    -package mutaskell -odir "$WORK" -hidir "$WORK" \
    "$ROOT/bench/diff-bench.hs" -o "$WORK/diff-bench"

run() {
    "$WORK/diff-bench" "$@"
}

echo "== one changed final line =="
for size in 2000 4000 8000 16000; do
    run single "$size"
done

echo "== many separated changes =="
for size in 2000 4000 8000 16000; do
    run many "$size"
done
