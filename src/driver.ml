(* Compiler driver: parse, typecheck, generate LLVM IR, link with the runtime *)

let read_program (file : string) : (Typechecker.cmodule list, string) result =
  match A2_parsing.parse_file file with
  | None -> Error (file ^ ": syntax error")
  | Some prog ->
      let load = A3_typechecking.loader_for (Filename.dirname file) in
      Typechecker.typecheck_program ~load prog

(* LLVM IR text for a program and everything it imports *)
let llvm_ir_of_file (file : string) : (string, string) result =
  match read_program file with
  | Error msg -> Error msg
  | Ok mods ->
      (try Ok (Codegen.generate mods) with Failure msg -> Error msg)

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let remove_quietly path = try Sys.remove path with Sys_error _ -> ()

(* compile [file] to the executable [out] with clang; the IR is kept next to
   it when [keep_ir] is set *)
let compile_file ?(keep_ir = false) ?(opt = "-O2") (file : string) (out : string)
  : (unit, string) result =
  match llvm_ir_of_file file with
  | Error msg -> Error msg
  | Ok ir ->
      let tmp = Filename.temp_file "cpe" ".ll" in
      let rt = Filename.temp_file "cpe_rt" ".c" in
      write_file tmp ir;
      write_file rt Rt_source.c_source;
      if keep_ir then write_file (out ^ ".ll") ir;
      let cmd = Printf.sprintf "clang %s -Wno-override-module -w %s %s -o %s -lm"
          opt (Filename.quote tmp) (Filename.quote rt) (Filename.quote out) in
      let status = Sys.command cmd in
      remove_quietly tmp;
      remove_quietly rt;
      if status = 0 then Ok () else Error "clang failed"
