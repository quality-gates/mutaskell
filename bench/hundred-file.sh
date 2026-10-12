#!/bin/sh
# Wall time of one project-mode mutation run over N tiny modules.
#
# The build and test commands are `true`, so the clock is discovery, generation,
# and the per-mutant process loop. A 10-file run must finish well under a
# 100-file run. The script prints an error count and the mutant count so a
# faster run that skipped the work is visible.
#
# Usage: bench/hundred-file.sh /path/to/mutaskell [files] [runs]
set -eu

BIN=${1:?usage: bench/hundred-file.sh /path/to/mutaskell [files] [runs]}
FILES=${2:-100}
RUNS=${3:-5}

if [ "$FILES" -lt 1 ] || [ "$RUNS" -lt 1 ]; then
    echo "files and runs must be at least 1" >&2
    exit 2
fi

ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

python3 - "$WORK/proj" "$FILES" <<'PY'
import os, sys
dest, count = sys.argv[1], int(sys.argv[2])
os.makedirs(os.path.join(dest, "src"), exist_ok=True)
with open(os.path.join(dest, "bench.cabal"), "w", encoding="utf-8") as cabal:
    cabal.write(
        "cabal-version: 2.4\n"
        "name: bench\n"
        "version: 0.1\n"
        "\n"
        "library\n"
        "    hs-source-dirs: src\n"
        "    exposed-modules: A\n"
    )
for i in range(count):
    folder = os.path.join(dest, "src", "d%d" % i)
    os.makedirs(folder, exist_ok=True)
    with open(os.path.join(folder, "M%d.hs" % i), "w", encoding="utf-8") as src:
        src.write(
            "module D%d where\n"
            "f%d :: Int -> Int\n"
            "f%d x = x + %d\n" % (i, i, i, i)
        )
PY

# One run. Prints: real_s files mutants errors killed alive skipped exit
run_once() {
    # A finished run records every file in .mutaskell/progress. The next run
    # would resume and evaluate nothing. Remove that state so each sample does
    # the same work.
    rm -rf "$WORK/proj/.mutaskell"
    out="$WORK/out"
    err="$WORK/err"
    python3 - "$BIN" "$WORK/proj" "$out" "$err" "$FILES" <<'PY'
import subprocess, sys, time, re
bin_path, proj, out_path, err_path, expect = sys.argv[1:]
expect = int(expect)
start = time.perf_counter()
proc = subprocess.run(
    [bin_path, proj, "--build-cmd", "true", "--test-cmd", "true", "--jobs", "1"],
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
)
elapsed = time.perf_counter() - start
stdout = proc.stdout.decode("utf-8", "replace")
stderr = proc.stderr.decode("utf-8", "replace")
open(out_path, "w", encoding="utf-8").write(stdout)
open(err_path, "w", encoding="utf-8").write(stderr)

def grab(pattern, text, default="0"):
    match = re.search(pattern, text)
    return match.group(1) if match else default

discovered = grab(r"Discovered (\d+) source file", stdout, "-1")
mutants = grab(r"Total mutants:\s*(\d+)", stdout, "0")
summary_errors = grab(r"Errors:\s*(\d+)", stdout, "-1")
killed = grab(r"Killed:\s*(\d+)/", stdout, "0")
alive = grab(r"Alive:\s*(\d+)/", stdout, "0")
skipped = grab(r"Skipped \(non-compilable\):\s*(\d+)", stdout, "0")
skip_lines = len(re.findall(r"(?m)^SKIP ", stderr))
exceptions = len(re.findall(r"(?m)^(?:mutaskell: )?[Uu]ncaught exception", stderr))
errors = skip_lines + exceptions
if proc.returncode != 0:
    errors += 1
if discovered != str(expect):
    errors += 1
if mutants == "0":
    errors += 1
if summary_errors not in ("0",):
    errors += 1
print(
    "%.4f %s %s %d %s %s %s %d"
    % (
        elapsed,
        discovered,
        mutants,
        errors,
        killed,
        alive,
        skipped,
        proc.returncode,
    )
)
PY
}

i=1
times=""
last=""
worst_errors=0
while [ "$i" -le "$RUNS" ]; do
    last=$(run_once)
    set -- $last
    echo "run=$i real_s=$1 files=$2 mutants=$3 errors=$4 killed=$5 alive=$6 skipped=$7 exit=$8"
    times="$times $1"
    if [ "$4" -gt "$worst_errors" ]; then
        worst_errors=$4
    fi
    i=$((i + 1))
done

python3 - "$times" "$worst_errors" "$FILES" "$RUNS" "$last" <<'PY'
import sys
times = [float(x) for x in sys.argv[1].split()]
errors = int(sys.argv[2])
files = sys.argv[3]
runs = sys.argv[4]
last = sys.argv[5].split()
ordered = sorted(times)
mid = ordered[len(ordered) // 2]
print(
    "files=%s runs=%s median_s=%.4f min_s=%.4f max_s=%.4f errors=%d mutants=%s killed=%s alive=%s skipped=%s"
    % (files, runs, mid, ordered[0], ordered[-1], errors, last[2], last[4], last[5], last[6])
)
if errors != 0:
    sys.exit(1)
PY
