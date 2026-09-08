#!/bin/sh
# Residency benchmark for project mode: peak RSS over increasing file counts at
# a fixed per-file mutant cap (the config default applies to every file).
#
# Completed-file mutant source must not accumulate in the summary state, so peak
# RSS stays roughly flat as the file count grows.  On a build that retains every
# file's results, peak RSS grows linearly with the file count.  Run it against
# two builds to compare them.
#
# Usage: bench/residency.sh /path/to/mutaskell
#
# The synthetic project gives every file a ~40KB source body with a handful of
# mutable literals, so each retained mutant carries a full-size source string
# and the run's build/test commands cost nothing (all mutants survive).
set -e

BIN=${1:?usage: bench/residency.sh /path/to/mutaskell}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

case "$(uname)" in
    Darwin)
        TIME="/usr/bin/time"
        TFLAG="-l"
        FIELD="maximum resident set size"
        DIV=1048576   # macOS reports bytes; normalise to MiB
        ;;
    Linux)
        TIME="/usr/bin/time"
        TFLAG="-v"
        FIELD="Maximum resident set size"
        DIV=1024      # GNU time reports KiB
        ;;
    *)
        echo "unsupported platform: $(uname)" >&2
        exit 1
        ;;
esac

gen_project() {
    dir=$1
    count=$2
    body=$(head -c 40000 /dev/zero | tr '\0' 'x')
    i=0
    while [ "$i" -lt "$count" ]; do
        {
            echo "module B$i where"
            echo ""
            echo "big :: String"
            echo "big = \"$body\""
            echo ""
            j=0
            while [ "$j" -lt 8 ]; do
                echo "f$j :: Int -> Int"
                echo "f$j x = x + $j"
                echo ""
                j=$((j + 1))
            done
        } > "$dir/B$i.hs"
        i=$((i + 1))
    done
}

for count in 10 20 40; do
    rm -rf "$WORK/proj"
    mkdir -p "$WORK/proj"
    gen_project "$WORK/proj" "$count"
    # Cheap commands keep evaluation instant, so the measurement isolates the
    # walker's own retention rather than toolchain noise.
    output=$("$TIME" "$TFLAG" "$BIN" "$WORK/proj" --build-cmd true --test-cmd true \
        2>&1 >/dev/null || true)
    # macOS puts the number first on the line, GNU time puts it last; pull the
    # first number off the matching line either way.
    rss=$(printf '%s\n' "$output" | grep -i "$FIELD" | tr -cs '0-9' ' ' | awk '{print $1}')
    if [ -z "$rss" ]; then
        echo "files=$count: could not read peak RSS" >&2
        exit 1
    fi
    echo "files=$count peak_rss=$((rss / DIV))MiB"
done