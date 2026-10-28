#!/usr/bin/env bash
# Time one mutaskell binary run on a fixed source file.
# The clock starts at exec of the binary and stops when it exits.
# cabal is used only to locate the binary, before the clock starts.
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
cd "$root"

repeats=5
target="$root/bench/hillclimb/Hundred.hs"
if [[ "${1:-}" == "small" ]]; then
  target="$root/bench/hillclimb/Small.hs"
elif [[ "${1:-}" == "hundred" || -z "${1:-}" ]]; then
  target="$root/bench/hillclimb/Hundred.hs"
else
  echo "usage: bench/hillclimb/measure.sh [hundred|small]" >&2
  exit 2
fi

if [[ -f "$HOME/.ghcup/env" ]]; then
  # shellcheck disable=SC1091
  source "$HOME/.ghcup/env"
fi

bin=$(cabal list-bin mutaskell)
if [[ ! -x "$bin" ]]; then
  echo "mutaskell binary is not executable: $bin" >&2
  exit 2
fi

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

parse_summary() {
  local file=$1
  local total errors skipped alive killed
  total=$(sed -n 's/^Total mutants: \([0-9][0-9]*\).*/\1/p' "$file" | head -n 1)
  errors=$(sed -n 's/^[[:space:]]*Errors: \([0-9][0-9]*\).*/\1/p' "$file" | head -n 1)
  skipped=$(sed -n 's/^[[:space:]]*Skipped (non-compilable): \([0-9][0-9]*\).*/\1/p' "$file" | head -n 1)
  alive=$(sed -n 's/^[[:space:]]*Alive: \([0-9][0-9]*\)\/.*/\1/p' "$file" | head -n 1)
  killed=$(sed -n 's/^[[:space:]]*Killed: \([0-9][0-9]*\)\/.*/\1/p' "$file" | head -n 1)
  if [[ -z "$total" || -z "$errors" || -z "$skipped" || -z "$alive" || -z "$killed" ]]; then
    echo "parse-failed"
    return
  fi
  echo "$total $errors $skipped $alive $killed"
}

times_file=$tmpdir/times
counts_file=$tmpdir/counts
: >"$times_file"
: >"$counts_file"

for i in $(seq 1 "$repeats"); do
  out=$tmpdir/out-$i
  err=$tmpdir/err-$i
  start=$(date +%s.%N)
  set +e
  "$bin" "$target" >"$out" 2>"$err"
  status=$?
  set -e
  end=$(date +%s.%N)
  elapsed=$(awk -v s="$start" -v e="$end" 'BEGIN { printf "%.6f", e - s }')
  parsed=$(parse_summary "$out" || true)
  echo "$elapsed" >>"$times_file"
  if [[ "$parsed" == "parse-failed" || -z "$parsed" ]]; then
    echo "run=$i status=$status seconds=$elapsed errors=? evaluated=? parse=failed" 
    echo "PARSE_FAIL $status" >>"$counts_file"
    echo "--- stdout ---" >&2
    cat "$out" >&2
    echo "--- stderr ---" >&2
    cat "$err" >&2
    exit 1
  fi
  read -r total errors skipped alive killed <<<"$parsed"
  evaluated=$total
  tested=$((alive + killed))
  echo "run=$i status=$status seconds=$elapsed evaluated=$evaluated tested=$tested killed=$killed alive=$alive errors=$errors skipped=$skipped"
  echo "$status $total $errors $skipped $alive $killed" >>"$counts_file"
done

sort -n "$times_file" >"$tmpdir/sorted"
min_s=$(head -n 1 "$tmpdir/sorted")
max_s=$(tail -n 1 "$tmpdir/sorted")
median_s=$(awk '
  { a[NR] = $1 }
  END {
    if (NR == 0) exit 1
    if (NR % 2 == 1) printf "%.6f", a[(NR + 1) / 2]
    else printf "%.6f", (a[NR / 2] + a[NR / 2 + 1]) / 2
  }
' "$tmpdir/sorted")
spread_s=$(awk -v min="$min_s" -v max="$max_s" 'BEGIN { printf "%.6f", max - min }')

first=$(head -n 1 "$counts_file")
read -r status total errors skipped alive killed <<<"$first"
unstable=0
while read -r st tot er sk al ki; do
  if [[ "$tot" != "$total" || "$er" != "$errors" || "$sk" != "$skipped" || "$al" != "$alive" || "$ki" != "$killed" || "$st" != "0" ]]; then
    unstable=1
  fi
done <"$counts_file"

echo "file=$(basename "$target")"
echo "binary=$bin"
echo "repeats=$repeats"
echo "status=$status"
echo "evaluated=$total"
echo "tested=$((alive + killed))"
echo "killed=$killed"
echo "alive=$alive"
echo "errors=$errors"
echo "skipped=$skipped"
echo "median_s=$median_s"
echo "min_s=$min_s"
echo "max_s=$max_s"
echo "spread_s=$spread_s"
echo "counts_stable=$([[ $unstable -eq 0 ]] && echo yes || echo no)"

if [[ "$unstable" -ne 0 ]]; then
  echo "mutant counts or exit status changed across repeats" >&2
  exit 1
fi
if [[ "$total" -lt 1 ]]; then
  echo "no mutants were evaluated" >&2
  exit 1
fi
