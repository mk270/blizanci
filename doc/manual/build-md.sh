#!/bin/sh
# Generate Markdown copies of the manual's Gemtext chapters.
#
# Usage: doc/manual/build-md.sh
#
# Reads doc/manual/*.gmi and writes doc/manual/md/*.md, using the
# gemtext2md filter (stdin to stdout).  Set GEMTEXT2MD to the
# filter's path if it is not on PATH.  The md directory is generated
# output and is not committed.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
out="$here/md"
conv=${GEMTEXT2MD:-gemtext2md}

if ! command -v "$conv" >/dev/null 2>&1; then
    echo "build-md.sh: $conv not found; put it on PATH" \
         "or set GEMTEXT2MD" >&2
    exit 1
fi

mkdir -p "$out"
for src in "$here"/*.gmi; do
    name=$(basename "$src" .gmi)
    tmp="$out/$name.md.tmp"
    # relative links to other chapters must point at the .md copies
    "$conv" < "$src" |
        sed 's/](\([A-Za-z0-9_-]*\)\.gmi)/](\1.md)/g' > "$tmp"
    mv "$tmp" "$out/$name.md"
done
