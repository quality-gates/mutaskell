#!/bin/sh
# Worker transport benchmark: end-to-end parallel evaluation of the example
# module through the CLI at several worker counts, reporting wall time and
# peak resident set size per run.
#
# Generation/transport and test execution are separate workloads and are not
# attributable inside a single run: the parent generates the candidate set
# once per run (one `genMutants`), and after #33 each worker child performs
# no generation at all — it evaluates the workload document it is handed.
# Before #33 every child re-ran the full generation pipeline, so the
# generation count per run was 1 + (number of mutants evaluated).  Compare
# this script's output between commits to see the removed generation cost;
# the example module is small, so most of its remaining time is test
# execution.
#
# Usage: bench/worker.sh
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
BIN=$(find "$ROOT/dist-newstyle/build" -type f -name mutaskell \
        -exec stat -f '%m %N' {} + | sort -rn | head -1 | cut -d' ' -f2-)

echo "== candidate population =="
"$BIN" --dry-run Examples/AssertCheckTest.hs | tail -2

echo "== end-to-end runs (wall time, peak RSS) =="
for workers in 1 2 4; do
    for repeat in 1 2 3; do
        /usr/bin/time -l "$BIN" Examples/AssertCheckTest.hs --workers "$workers" --quiet \
            2>&1 >/dev/null | awk -v w="$workers" -v r="$repeat" \
            '/real/       {wall = $1}
             /maximum resident set size/ {rss = $1}
             END {printf "workers=%s repeat=%s wall=%ss peak_rss=%sKB\n", w, r, wall, rss}'
    done
done