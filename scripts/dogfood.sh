#!/usr/bin/env bash
set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
mkdir -p "$repo/.mutaskell"
report=$(mktemp -d "$repo/.mutaskell/dogfood.XXXXXX")
work=$(mktemp -d "${TMPDIR:-/tmp}/mutaskell-dogfood.XXXXXX")
cleanup() {
  if [[ -d "$work/.mutaskell" ]]; then
    cp -R "$work/.mutaskell" "$report/run-state"
  fi
  rm -rf "$work"
  echo "Dogfood evidence: $report"
}
trap cleanup EXIT

# Each run gets fresh progress and a private source tree.
rsync -a --exclude='.git' --exclude='dist-newstyle' --exclude='.stack-work' \
  --exclude='.mutaskell' --exclude='.mutants' \
  --exclude='.ghc.environment.*' --exclude='cabal.project.local' \
  "$repo/" "$work/"
cd "$work"
find src app -type f \( -name '*.hs' -o -name '*.lhs' \) | LC_ALL=C sort > "$report/sources.txt"
if [[ ! -s "$report/sources.txt" ]]; then
  echo 'No production Haskell sources found.' >&2
  exit 1
fi

# Keep local Fleet-host limits; GitHub's isolated runner can run the full suite.
mutant_timeout=30
project_budget=1800
if [[ "${GITHUB_ACTIONS:-}" == true ]]; then
  mutant_timeout=900
  project_budget=18000
fi

build='cabal build --write-ghc-environment-files=always all test:spec'
test='cabal test spec --test-show-details=direct'
$build > "$report/build.log" 2>&1
# Mutating app/Main.hs rebuilds the executable. Keep the evaluator outside that path.
cp "$(cabal list-bin exe:mutaskell)" "$work/dogfood-runner"
printf '{}\n' > "$work/dogfood-config.yaml"

set +e
"$work/dogfood-runner" . --config "$work/dogfood-config.yaml" \
  --only-files "$report/sources.txt" --result-out "$report/counts.txt" \
  --workers 1 --jobs 1 --timeout "$mutant_timeout" --time-budget "$project_budget" \
  --build-cmd "$build" --test-cmd "$test" --min-msi 80 \
  2>&1 | tee "$report/mutation.log"
statuses=("${PIPESTATUS[@]}")
set -e
if [[ "${statuses[0]}" -ne 0 ]]; then
  exit "${statuses[0]}"
fi
if [[ "${statuses[1]}" -ne 0 ]]; then
  exit "${statuses[1]}"
fi

# Project mode can otherwise exit successfully after skipping a bad source or
# stopping early. --only-files retains its progress ledger for this check.
if grep -q '^SKIP ' "$report/mutation.log"; then
  echo 'Production sources were skipped; the mutation gate is incomplete.' >&2
  exit 1
fi
if [[ ! -f .mutaskell/progress ]]; then
  echo 'Missing completion evidence for production mutation testing.' >&2
  exit 1
fi
LC_ALL=C sort -u .mutaskell/progress > "$report/completed.txt"
if ! diff -u "$report/sources.txt" "$report/completed.txt"; then
  echo 'Every production source must finish before the mutation gate can pass.' >&2
  exit 1
fi
if [[ ! -s "$report/counts.txt" ]]; then
  echo 'Missing mutation results.' >&2
  exit 1
fi
echo 'Production Haskell passed the 80% MSI gate.'
