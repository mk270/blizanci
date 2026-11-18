#!/bin/sh
# Generate Markdown copies of the manual's Gemtext chapters.
#
# Usage: doc/manual/build-md.sh
#
# Reads doc/manual/*.gmi and writes doc/manual/md/*.md.  The md
# directory is generated output and is not committed.
#
# The converter is the gemtext2md escript from
# https://github.com/mk270/erl_gemtext2md (needs Erlang/OTP 27 or
# later on the PATH).  By default the pinned release below is
# downloaded once into _build/tools/ and its sha256 is checked.  Set
# GEMTEXT2MD to the path of a converter (a filter: stdin to stdout) to
# use that instead, e.g. a local build.

set -eu

# To upgrade: set the new version and the sha256 from the release's
# gemtext2md.sha256 file.
vsn=0.1.1
sha256=3b55a60f6fcd1845e77e074aa341b74bea9e1481dbac4d06f1868b71b64ee15c
url=https://github.com/mk270/erl_gemtext2md/releases/download
url="$url/v$vsn/gemtext2md"

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out="$here/md"

if [ -n "${GEMTEXT2MD:-}" ]; then
    conv=$GEMTEXT2MD
else
    conv="$root/_build/tools/gemtext2md-$vsn"
    if [ ! -x "$conv" ]; then
        command -v curl >/dev/null 2>&1 || {
            echo "build-md.sh: curl not found" >&2; exit 1; }
        command -v sha256sum >/dev/null 2>&1 || {
            echo "build-md.sh: sha256sum not found" >&2; exit 1; }
        mkdir -p "$root/_build/tools"
        tmp="$conv.download"
        curl -fsSL -o "$tmp" "$url"
        if ! echo "$sha256  $tmp" | sha256sum -c - >/dev/null 2>&1; then
            rm -f "$tmp"
            echo "build-md.sh: checksum mismatch for $url" >&2
            exit 1
        fi
        chmod +x "$tmp"
        mv "$tmp" "$conv"
    fi
fi

if ! command -v "$conv" >/dev/null 2>&1 && [ ! -x "$conv" ]; then
    echo "build-md.sh: converter $conv not found" >&2
    exit 1
fi
if ! command -v escript >/dev/null 2>&1 && [ -z "${GEMTEXT2MD:-}" ]; then
    echo "build-md.sh: escript not found; put Erlang/OTP 27 or" \
         "later on the PATH" >&2
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
