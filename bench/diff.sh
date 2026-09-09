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

record() {
    result_file=$1
    shift
    benchmark_result=$(run "$@")
    printf '%s\n' "$benchmark_result"
    printf '%s\n' "$benchmark_result" >> "$result_file"
}

assert_linear() {
    label=$1
    result_file=$2
    first=$(sed -n '1s/.*allocated_bytes=\([0-9][0-9]*\).*/\1/p' "$result_file")
    last=$(tail -n 1 "$result_file" | sed -n 's/.*allocated_bytes=\([0-9][0-9]*\).*/\1/p')
    awk -v label="$label" -v first="$first" -v last="$last" '
        BEGIN {
            # The sweep grows by 8x.  A 16x allocation ceiling allows noisy
            # machines some headroom while rejecting quadratic growth.
            if (first == "" || last == "" || first <= 0 || last > first * 16) {
                printf "%s allocation scaling exceeded 16x over an 8x input sweep\\n", label > "/dev/stderr"
                exit 1
            }
        }'
}

single_results="$WORK/single.results"
echo "== one changed final line =="
for size in 2000 4000 8000 16000; do
    record "$single_results" single "$size"
done
assert_linear "single-change" "$single_results"

many_results="$WORK/many.results"
echo "== many separated changes =="
for size in 2000 4000 8000 16000; do
    record "$many_results" many "$size"
done
assert_linear "many-change" "$many_results"
