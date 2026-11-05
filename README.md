
Phase 4 - Code generation (LLVM)
Compile a program (and the files it imports) to a native executable:
  dune build
  alias cpec=_build/default/bin/cpec.exe
  _build/default/bin/cpec.exe tests/04-program/add.cpe -o add
  ./add 2 3            # prints 5
  _build/default/bin/cpec.exe prog.cpe --emit-llvm   # print the LLVM IR instead
Needs clang on the PATH. The compiler emits textual LLVM IR (src/codegen.ml)
and clang links it with the C runtime in runtime/cpe_rt.c, which is embedded in
the compiler. The runtime has no garbage collector. main's result is printed
followed by a newline; a run-time error (error, non-exhaustive match, division
by zero) prints "error: ..." on stderr and exits with status 1.
