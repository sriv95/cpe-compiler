#!/bin/sh
# Usage: bin/typecheck.sh [file-or-glob ...]
# Default: every .cpe under tests/03-typechecking/

cd "$(dirname "$0")/.." || exit 1

if [ $# -eq 0 ]; then
  set -- tests/03-typechecking/*/*.cpe
fi

# run dune script; typecheck_file catches per-file exceptions so all files run
for f in "$@"; do
  echo "CpecmuCompiler.A3_typechecking.typecheck_file \"$f\";;"
done | dune utop src -- -stdin 2>&1 | grep -E '^(tests|examples|/|\.).*\.cpe: |^Exception'
