(** header: optional imports & setup **)
{
open Token

let error msg lexbuf =
  Error(msg, Lexing.lexeme_start_p lexbuf)

let string_buf = Buffer.create 256
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+
let symbols = "+" | "*" | "/" | "//" | "%" | "<" | "<=" | ">=" | ">" | "==" | "!=" | "=" | "&&" | "||" | "|" | "->" | "::" | "(" | ")" | ":"
let boolLit = "True" | "False"
let digit = ['0'-'9']
let intLit = digit+ | '-'digit+
let floatLit = ('-'digit+ | digit+)'.'digit+
let letter = ['a'-'z' 'A'-'Z']
let escapeChars = '\\' ('\\' | '\'' | '"' | 'n' | 'r' | 't' | 'b')
let charLit = '\'' (letter | escapeChars) '\''
let comment = '#' [^ '\r' '\n']* (line_terminator | eof)

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | comment { read lexbuf }
  | symbols { Symbol(Lexing.lexeme lexbuf) }
  | boolLit { BoolLit (bool_of_string (Lexing.lexeme lexbuf)) }
  | intLit { IntLit (int_of_string (Lexing.lexeme lexbuf)) }
  | floatLit { FloatLit (float_of_string (Lexing.lexeme lexbuf))}
  | charLit { CharLit (Lexing.lexeme_char lexbuf 1) }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
