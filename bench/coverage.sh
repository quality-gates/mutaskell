#!/bin/sh
# Coverage benchmark: force disjoint-span reduction and indexed containment
# queries over growing inputs.  Project-level tix read reuse is verified by the
# instrumented multi-file ProjectSpec test.
#
# Usage: bench/coverage.sh
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
cabal exec -- ghc -O1 -rtsopts -package mutaskell \
    -odir "$WORK" -hidir "$WORK" "$ROOT/bench/coverage-bench.hs" \
    -o "$WORK/coverage-bench"

run() {
    "$WORK/coverage-bench" "$@"
}

echo "== disjoint uncovered-span reduction =="
for size in 2000 4000 8000 16000; do
    run sweep "$size"
done

echo "== indexed candidate containment =="
for size in 2000 4000 8000 16000; do
    run query "$size"
done
