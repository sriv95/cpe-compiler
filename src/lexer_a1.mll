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
let symbols = "+' | "*" | "/" | "//" | "%" | "<" | "<=" | ">=" | ">" | "==" | "!=" | "="
let digit = ['0'-'9']
let digits = digit+ | '-'digit+

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | "+" { Symbol("+") }
  | digits { IntLit (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
