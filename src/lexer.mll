(** header: optional imports & setup **)
{
open Parser

exception SyntaxError of string * Lexing.position
let error msg lexbuf =
  raise (SyntaxError(msg, Lexing.lexeme_start_p lexbuf))

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+
let digit = ['0'-'9']
let intLit = digit+
let floatLit = digit+'.'digit+
let letter = ['a'-'z' 'A'-'Z']
let idChar = letter | digit | '_'
let varName = ['a'-'z'] idChar*
let typeName = ['A'-'Z'] idChar*
let escapeChars = '\\' ('\\' | '\'' | '"' | 'n' | 'r' | 't' | 'b')
let charLit = '\'' ([^ '\'' '\\' '\r' '\n'] | escapeChars) '\''
let unknownEscapeChar = '\'' '\\' _ '\''
let stringLit = '"' ([^ '"' '\\' '\r' '\n'] | escapeChars)* '"'
let comment = '#' [^ '\r' '\n']* (line_terminator | eof)

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "||" { OROR }
  | "&&" { ANDAND }
  | varName as x { VAR x }
  | typeName as x { NAME x }
  | intLit as x { INT_LIT (int_of_string x) }
  | floatLit as x { FLOAT_LIT (float_of_string x) }
  | charLit {
      let s = Lexing.lexeme lexbuf in
      let c = if String.length s = 3 then s.[1]
        else match s.[2] with
          | 'n' -> '\n'
          | 't' -> '\t'
          | 'r' -> '\r'
          | 'b' -> '\b'
          | c -> c
      in
      CHAR_LIT c
    }
  | unknownEscapeChar { error (Lexing.lexeme lexbuf) lexbuf }
  | stringLit {
      let s = Lexing.lexeme lexbuf in
      let n = String.length s - 1 in
      let b = Buffer.create n in
      let rec go i =
        if i < n then
          if s.[i] = '\\' then begin
            Buffer.add_char b
              (match s.[i + 1] with
               | 'n' -> '\n'
               | 't' -> '\t'
               | 'r' -> '\r'
               | 'b' -> '\b'
               | c -> c);
            go (i + 2)
          end else (Buffer.add_char b s.[i]; go (i + 1))
      in
      go 1;
      STRING_LIT (Buffer.contents b)
    }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
