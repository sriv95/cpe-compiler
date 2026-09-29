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

let keyword = "def" | "import" | "as" | "match" | "with" | "if" | "then" | "else" | "let" | "in" | "and" | "data" | "type"
let symbols = "+" | "*" | "/" | "//" | "%" | "<" | "<=" | ">=" | ">" | "==" | "!=" | "=" | "&&" | "||" | "|" | "->" | "::" | "(" | ")" | ":"
let boolLit = "True" | "False"
let digit = ['0'-'9']
let intLit = digit+
let floatLit = digit+'.'digit+
let letter = ['a'-'z' 'A'-'Z']
let escapeChars = '\\' ('\\' | '\'' | '"' | 'n' | 'r' | 't' | 'b')
let charLit = '\'' (letter | escapeChars) '\''
let comment = '#' [^ '\r' '\n']* (line_terminator | eof)

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | comment { read lexbuf }
  | keyword { Keyword(Lexing.lexeme lexbuf) }
  | symbols { Symbol(Lexing.lexeme lexbuf) }
  | boolLit { BoolLit (Lexing.lexeme lexbuf = "True") }
  | intLit { IntLit (int_of_string (Lexing.lexeme lexbuf)) }
  | floatLit { FloatLit (float_of_string (Lexing.lexeme lexbuf))}
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
      CharLit c
    }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }
