(* cpec: compile a CPE program to a native executable through LLVM *)
open CpecmuCompiler

let usage () =
  prerr_endline "usage: cpec <file.cpe> [-o <output>] [--emit-llvm]";
  prerr_endline "  -o <output>   name of the executable (default: the file name without .cpe)";
  prerr_endline "  --emit-llvm   print LLVM IR to standard output instead of compiling";
  exit 2

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let rec parse file out emit = function
    | [] -> (file, out, emit)
    | "-o" :: o :: rest -> parse file (Some o) emit rest
    | "--emit-llvm" :: rest -> parse file out true rest
    | f :: rest when file = None && f <> "" && f.[0] <> '-' -> parse (Some f) out emit rest
    | _ -> usage ()
  in
  match parse None None false args with
  | None, _, _ -> usage ()
  | Some file, out, emit ->
      let fail msg = prerr_endline msg; exit 1 in
      if emit then
        (match Driver.llvm_ir_of_file file with
         | Ok ir -> print_string ir
         | Error msg -> fail msg)
      else begin
        let out = match out with
          | Some o -> o
          | None -> Filename.remove_extension (Filename.basename file) in
        match Driver.compile_file file out with
        | Ok () -> ()
        | Error msg -> fail msg
      end
