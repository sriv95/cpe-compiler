#!/bin/sh
# Usage: bin/parse.sh [file-or-glob ...]
# Default: every .cpe under tests/02-parsing/

cd "$(dirname "$0")/.." || exit 1

if [ $# -eq 0 ]; then
  set -- tests/03-typechecking/*/*.cpe
fi

# run dune script
for f in "$@"; do
  echo "CpecmuCompiler.A3_typechecking.typecheck_file \"$f\";;"
done | dune utop src -- -stdin >/dev/null || exit 1

# print written output
# for f in "$@"; do
#   echo "=== File: $f"
#   cat "out/03-typechecking/$f"
# done
