#!/usr/bin/env bash
set -uo pipefail
cd /work
cmd=$(<.explore/test-cmd.txt)
for jobs in 1 2 2; do
  root=".explore/args-$jobs"
  rm -rf "$root"
  mkdir -p "$root"
  printf 'packages: .\n' > "$root/cabal.project"
  printf 'module M1 where\nf :: Int -> Int\nf x = x + 1\ng :: Int -> Int\ng x = x * 2\nh :: Int -> Bool\nh x = x > 0\n' > "$root/M1.hs"
  printf '\n===== jobs=%s =====\n' "$jobs"
  timeout 300 cabal run --enable-tests mutaskell -- "$root" --build-cmd true --test-cmd "$cmd" --test-args 'space value' --test-args marker --test-args "O'Brien" --max-mutants 1 --workers 1 --jobs "$jobs" --timeout 30 --time-budget 1800
  printf 'EXIT jobs=%s: %s\n' "$jobs" "$?"
done
