#!/bin/sh
# Worker transport benchmark: end-to-end parallel evaluation of the example
# module through the CLI at several worker counts, reporting generation
# count, throughput and peak resident set size per run.
#
# Generation/transport and test execution are separate workloads and are
# reported separately below.  The generation-only rows time `--dry-run`,
# which generates candidates and stops before any test runs.  After #33 the
# parent generates the candidate set once per run (one trace line) and each
# worker child performs no generation at all: it evaluates the workload
# document it is handed.  Before #33 every child re-ran the full generation
# pipeline, so the generation count per run was 1 + (number of mutants
# evaluated); MUCHECK_TRACE counts the actual invocations.  Compare this
# script's output between commits to see the removed generation cost; the
# example module is small, so most of its remaining time is test execution.
#
# The workers=1 row exercises the in-process serial path (--workers 1 does
# not fork children), while 2 and 4 run the workload-transport subprocess
# path; the three agree on outcomes (see the worker spec).
#
# macOS/BSD stat and /usr/bin/time -l are used, with a GNU stat fallback.
#
# Usage: bench/worker.sh
set -e

ROOT=$(cd "$(dirname "$0")/.." && pwd)

cd "$ROOT"
cabal build --write-ghc-environment-files=always all >/dev/null
STAT_FMT='%m %N'
BIN=$(find "$ROOT/dist-newstyle/build" -type f -name mutaskell \
        -exec stat -f "$STAT_FMT" {} + 2>/dev/null | sort -rn | head -1 | cut -d' ' -f2-)
if [ -z "$BIN" ]; then
    STAT_FMT='%Y %n'
    BIN=$(find "$ROOT/dist-newstyle/build" -type f -name mutaskell \
            -exec stat -c "$STAT_FMT" {} + 2>/dev/null | sort -rn | head -1 | cut -d' ' -f2-)
fi

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

echo "== generation only (--dry-run: generation, no test execution) =="
"$BIN" --dry-run Examples/AssertCheckTest.hs | tail -2
for repeat in 1 2 3; do
    /usr/bin/time -l env MUCHECK_TRACE=1 "$BIN" --dry-run Examples/AssertCheckTest.hs --quiet \
        2>"$WORK/err" >/dev/null
    gens=$(grep -c 'trace: generation invocation' "$WORK/err" || true)
    awk -v r="$repeat" -v g="$gens" '/real/ {wall = $1}
        END {printf "repeat=%s wall=%ss generations=%s\n", r, wall, g}' "$WORK/err"
done

echo "== end-to-end runs (generation count, throughput, wall time, peak RSS) =="
for workers in 1 2 4; do
    for repeat in 1 2 3; do
        summary="$WORK/summary-$workers-$repeat.json"
        /usr/bin/time -l env MUCHECK_TRACE=1 "$BIN" Examples/AssertCheckTest.hs \
            --workers "$workers" --quiet --logger-json "$summary" \
            2>"$WORK/err" >/dev/null
        gens=$(grep -c 'trace: generation invocation' "$WORK/err" || true)
        total=$(grep -o '"total"[[:space:]]*:[[:space:]]*[0-9]*' "$summary" | head -1 \
                  | grep -o '[0-9]*')
        awk -v w="$workers" -v r="$repeat" -v g="$gens" -v n="$total" \
            '/real/       {wall = $1}
             /maximum resident set size/ {rss = $1}
             END {printf "workers=%s repeat=%s wall=%ss peak_rss=%sKB generations=%s throughput=%s mutants/s\n",
                  w, r, wall, rss, g, (n / wall)}' "$WORK/err"
    done
done