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
  | "(" { LPAREN }
  | ")" { RPAREN }
  | intLit as x { INT_LIT (int_of_string x) }
  | "true" { TRUE }
  | "false" { FALSE }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
