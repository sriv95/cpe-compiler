#!/bin/sh
# Usage: bin/lex_a1.sh [file-or-glob ...]
# Default: every .cpe under tests/01-lexing/

cd "$(dirname "$0")/.." || exit 1

if [ $# -eq 0 ]; then
  set -- tests/01-lexing/*/*.cpe
fi

# run dune script
for f in "$@"; do
  echo "CpecmuCompiler.A1_lexing.lex_test \"$f\";;"
done | dune utop src -- -stdin >/dev/null || exit 1

# print written output
for f in "$@"; do
  echo "=== File: $f"
  cat "out/01-lexing/$f"
done
