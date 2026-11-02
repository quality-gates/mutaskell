#!/usr/bin/env bash
set -uo pipefail
cd /work
E=/work/.explore/evidence
mkdir -p "$E"
step() {
  local label=$1
  shift
  printf '\n===== %s =====\n' "$label"
  "$@"
  local rc=$?
  printf 'EXIT %s: %s\n' "$label" "$rc"
  return "$rc"
}
step versions bash -c 'ghc --version; cabal --version; df -h /work' || exit 1
step update timeout 300 cabal update || exit 1
step freeze timeout 600 cabal freeze --enable-tests || exit 1
cp cabal.project.freeze "$E/cabal.project.freeze"
sha256sum cabal.project.freeze > "$E/freeze.sha256"
step build timeout 2400 cabal build --enable-tests --write-ghc-environment-files=always all || exit 1
step freeze_unchanged sha256sum -c "$E/freeze.sha256" || exit 1
step tests timeout 1800 cabal test --enable-tests all --test-show-details=direct || true
step dry_run timeout 600 cabal run --enable-tests mutaskell -- --dry-run Examples/AssertCheckTest.hs --workers 1 --timeout 30 || true
step smoke timeout 1800 cabal run --enable-tests mutaskell -- Examples/AssertCheckTest.hs --workers 1 --timeout 30 || true
step baseline_create timeout 1800 cabal run --enable-tests mutaskell -- Examples/AssertCheckTest.hs --update-baseline .explore/baseline.txt --logger-json .explore/create.json --workers 1 --timeout 30 || true
if [ -f .explore/baseline.txt ]; then
  cp .explore/baseline.txt "$E/baseline-before.txt"
  step baseline_update timeout 1800 cabal run --enable-tests mutaskell -- Examples/AssertCheckTest.hs --baseline .explore/baseline.txt --update-baseline .explore/baseline.txt --logger-json .explore/update.json --workers 1 --timeout 30 || true
  cp .explore/baseline.txt "$E/baseline-after.txt"
  step baseline_compare cmp "$E/baseline-before.txt" "$E/baseline-after.txt" || true
  cp .explore/create.json .explore/update.json "$E/" 2>/dev/null || true
fi
step haddock timeout 1800 cabal haddock --enable-tests all || true
step freeze_final sha256sum -c "$E/freeze.sha256" || true
