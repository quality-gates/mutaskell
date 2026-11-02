#!/usr/bin/env bash
set -uo pipefail
cd /work
export PATH="/work/.explore/tools/usr/bin:$PATH"
export LD_LIBRARY_PATH="/work/.explore/tools/usr/lib/x86_64-linux-gnu${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
step() {
  local label=$1
  shift
  printf '\n===== %s =====\n' "$label"
  "$@"
  local rc=$?
  printf 'EXIT %s: %s\n' "$label" "$rc"
  return "$rc"
}
step rsync_version rsync --version || exit 1
step project_args bash .explore/replay-args.sh || true
step replay_test timeout 600 cabal test --enable-tests spec --test-show-details=direct --test-options='--match "passes test arguments through the project worker command"' || true
step full_tests timeout 2400 cabal test --enable-tests all --test-show-details=direct || true
step freeze_unchanged sha256sum -c .explore/evidence/freeze.sha256 || true
