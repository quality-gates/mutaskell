#!/usr/bin/env bash
set -uo pipefail
cd /work
export PATH="/work/.explore/tools/usr/bin:$PATH"
export LD_LIBRARY_PATH="/work/.explore/tools/usr/lib/x86_64-linux-gnu${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
events() {
  if [ -r /sys/fs/cgroup/memory.events ]; then
    printf '%s\n' 'cgroup memory.events:'
    while IFS= read -r line; do printf '%s\n' "$line"; done < /sys/fs/cgroup/memory.events
  fi
}
events
printf '\n===== full_tests_bounded_heap =====\n'
timeout 3000 cabal test --enable-tests --ghc-options=-rtsopts all --test-show-details=direct --test-options='+RTS -M2G -RTS'
printf 'EXIT full_tests_bounded_heap: %s\n' "$?"
events
printf '\n===== freeze_unchanged =====\n'
sha256sum -c .explore/evidence/freeze.sha256
printf 'EXIT freeze_unchanged: %s\n' "$?"
