#!/bin/sh
# Discovery benchmark for project mode: wall time of discovery, resume, worker
# sharding and CPP macro discovery over growing workload sizes.  Run it against
# two builds to compare them.
#
# What each scenario measures:
#   * roots    — discovery over overlapping roots (a root cabal file declares
#                hs-source-dirs src, so the package-dir root subsumes it).  A
#                build that walks every root re-lists the whole tree per root;
#                the change walks it once, so time stays flat as dirs grow.
#   * resume   — a second run whose .mutaskell/progress already holds every
#                discovered file.  Isolates discovery + resume filtering
#                (build and test commands are `true`, nothing pending).
#   * workers  — a --jobs N run over the same project; sharding must touch
#                each file once per run, not once per bucket.
#   * cpp      — K CPP modules over a build tree of D directories holding one
#                cabal_macros.h.  A build that scans per CPP parse re-walks the
#                build tree K times; the change scans once per project.
#
# Usage: bench/discovery.sh /path/to/mutaskell
set -e

BIN=${1:?usage: bench/discovery.sh /path/to/mutaskell}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

CPUS=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)

# Wall-clock seconds of "$@" via /usr/bin/time -p (POSIX output).
run_timed() {
    out=$(/usr/bin/time -p "$@" 2>&1 >/dev/null || true)
    printf '%s\n' "$out" | awk '/^real/ { print $2 }'
}

# A root cabal file whose package dir (".") subsumes the declared src root.
gen_rooted_project() {
    dir=$1
    count=$2
    mkdir -p "$dir/src"
    cat > "$dir/bench.cabal" <<EOF
cabal-version: 2.4
name: bench
version: 0.1

library
    hs-source-dirs: src
    exposed-modules: A
EOF
    i=0
    while [ "$i" -lt "$count" ]; do
        mkdir -p "$dir/src/d$i"
        {
            echo "module D$i where"
            echo "f$i :: Int -> Int"
            echo "f$i x = x + $i"
        } > "$dir/src/d$i/M$i.hs"
        i=$((i + 1))
    done
}

# Mark every file under $dir as already done, so the next run only pays
# discovery + resume filtering.
mark_all_done() {
    dir=$1
    mkdir -p "$dir/.mutaskell"
    ( cd "$dir" && find . -name '*.hs' | sed 's|^\./||' ) > "$dir/.mutaskell/progress"
}

# A build tree of $3 directories with one cabal_macros.h, plus $2 CPP modules
# that need it.
gen_cpp_project() {
    dir=$1
    count=$2
    dirs=$3
    mkdir -p "$dir"
    i=0
    while [ "$i" -lt "$dirs" ]; do
        mkdir -p "$dir/dist-newstyle/store/d$i"
        i=$((i + 1))
    done
    echo "#ifndef BENCH_MACROS_H" > "$dir/dist-newstyle/store/cabal_macros.h"
    echo "#define BENCH_MACROS_H 1" >> "$dir/dist-newstyle/store/cabal_macros.h"
    echo "#endif" >> "$dir/dist-newstyle/store/cabal_macros.h"
    i=0
    while [ "$i" -lt "$count" ]; do
        {
            echo "{-# LANGUAGE CPP #-}"
            echo "module Cpp$i where"
            echo "#if MIN_VERSION_base(4,16,0)"
            echo "f$i :: Int -> Int"
            echo "f$i x = x + $i"
            echo "#endif"
        } > "$dir/Cpp$i.hs"
        i=$((i + 1))
    done
}

echo "== discovery over overlapping roots (dry run) =="
for count in 50 100 200; do
    rm -rf "$WORK/proj"
    gen_rooted_project "$WORK/proj" "$count"
    t=$(run_timed "$BIN" "$WORK/proj" --dry-run)
    echo "files=$count real=${t}s"
done

echo "== resume with every file already done =="
for count in 50 100 200; do
    rm -rf "$WORK/proj"
    gen_rooted_project "$WORK/proj" "$count"
    mark_all_done "$WORK/proj"
    t=$(run_timed "$BIN" "$WORK/proj" --build-cmd true --test-cmd true)
    echo "files=$count real=${t}s"
done

echo "== worker sharding (--jobs $CPUS) =="
count=100
rm -rf "$WORK/proj"
gen_rooted_project "$WORK/proj" "$count"
t=$(run_timed "$BIN" "$WORK/proj" --build-cmd true --test-cmd true --jobs "$CPUS")
echo "files=$count jobs=$CPUS real=${t}s"

echo "== CPP macro discovery over a wide build tree =="
for count in 10 20 40; do
    rm -rf "$WORK/proj"
    gen_cpp_project "$WORK/proj" "$count" 2000
    t=$(run_timed "$BIN" "$WORK/proj" --dry-run)
    echo "cpp_files=$count real=${t}s"
done