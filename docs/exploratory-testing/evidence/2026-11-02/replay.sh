#!/usr/bin/env bash
set -uo pipefail
cd /work
E=/work/.explore/evidence
step() {
  local label=$1
  shift
  printf '\n===== %s =====\n' "$label"
  "$@"
  local rc=$?
  printf 'EXIT %s: %s\n' "$label" "$rc"
  return "$rc"
}
for i in 1 2; do
  step "replay_test_$i" timeout 600 cabal test --enable-tests spec --test-show-details=direct --test-options='--match "passes test arguments through the project worker command"' || true
done
mkdir -p .explore/project
printf 'packages: .\n' > .explore/project/cabal.project
printf 'module M1 where\nf :: Int -> Int\nf x = x + 1\n' > .explore/project/M1.hs
sha256sum .explore/project/M1.hs > "$E/project-before.sha256"
step project_dry timeout 600 cabal run --enable-tests mutaskell -- .explore/project --dry-run --workers 1 --jobs 1 --timeout 30 --time-budget 1800 || true
step project_run timeout 600 cabal run --enable-tests mutaskell -- .explore/project --build-cmd true --test-cmd true --max-mutants 1 --workers 1 --jobs 1 --timeout 30 --time-budget 1800 || true
step project_source_unchanged sha256sum -c "$E/project-before.sha256" || true
if [ -e .explore/project/.mutaskell/progress ]; then echo 'project progress: present'; else echo 'project progress: absent'; fi
step freeze_unchanged sha256sum -c "$E/freeze.sha256" || true
