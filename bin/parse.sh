#!/bin/sh
# Usage: bin/parse.sh [file-or-glob ...]
# Default: every .cpe under tests/02-parsing/

cd "$(dirname "$0")/.." || exit 1

if [ $# -eq 0 ]; then
  set -- tests/02-parsing/*/*.cpe
fi

# run dune script
for f in "$@"; do
  echo "CpecmuCompiler.A2_parsing.parse_test \"$f\";;"
done | dune utop src -- -stdin >/dev/null || exit 1

# print written output
for f in "$@"; do
  echo "=== File: $f"
  cat "out/02-parsing/$f"
done
