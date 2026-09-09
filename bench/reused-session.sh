#!/bin/sh
# Session-reuse benchmark for issue #36: one hint session per mutant versus one
# per test, over growing ordered test counts, for all-survivor mutants and
# mutants killed by the last test.  Three trials per cell, forced results.
#
# The bench binary is throwaway instrumentation for the investigation; it is
# not wired into the cabal file or the CLI.
#
# Usage: bench/reused-session.sh
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
# The interpreter resolves library modules through the package environment file
# written above, so the run must stay in the repo root.
cabal exec -- ghc -O1 -rtsopts -with-rtsopts=-T -package mutaskell \
    -odir "$WORK" -hidir "$WORK" "$ROOT/bench/reused-session-bench.hs" \
    -o "$WORK/reused-session-bench"

run() {
    "$WORK/reused-session-bench" "$@"
}

for mode in survivor late-kill; do
    echo "== $mode =="
    for size in 1 2 4 8 16 32; do
        for policy in fresh reused; do
            for trial in 1 2 3; do
                run "$mode" "$size" "$policy" "$trial"
            done
        done
    done
done
