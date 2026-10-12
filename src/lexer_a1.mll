(** header: optional imports & setup **)
{
open Token

let error msg lexbuf =
  Error(msg, Lexing.lexeme_start_p lexbuf)
}

(** regex patterns **)
let line_terminator = '\r' | '\n' | "\r\n"
let whitespace = (line_terminator | [' ' '\t'])+

let keyword = "def" | "import" | "as" | "match" | "with" | "if" | "then" | "else" | "let" | "in" | "and" | "data" | "type"
let symbols = "+" | "*" | "/" | "//" | "%" | "<" | "<=" | ">=" | ">" | "==" | "!=" | "=" | "&&" | "||" | "|" | "->" | "::" | "(" | ")" | ":" | "-" | "++" | "." | "$" | "[" | "]" | "," | "_" | "\\" | "=>" | "!"
let boolLit = "True" | "False"
let digit = ['0'-'9']
let intLit = digit+
let floatLit = digit+'.'digit+
let letter = ['a'-'z' 'A'-'Z']
let escapeChars = '\\' ('\\' | '\'' | '"' | 'n' | 'r' | 't' | 'b')
let charLit = '\'' ([^ '\'' '\\' '\r' '\n'] | escapeChars) '\''
let stringLit = '"' ([^ '"' '\\' '\r' '\n'] | escapeChars)* '"'
let comment = '#' [^ '\r' '\n']* (line_terminator | eof)

(** lexing rules **)
rule read = parse
  | whitespace { read lexbuf }
  | comment { read lexbuf }
  | "(*" { comment_ml (Lexing.lexeme_start_p lexbuf) lexbuf }
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
      StringLit (Buffer.contents b)
    }
  | eof { EOF }
  | _ { error (Lexing.lexeme lexbuf) lexbuf }

and comment_ml start = parse
  | "*)" { read lexbuf }
  | "(*" { Error("nested comment", Lexing.lexeme_start_p lexbuf) }
  | line_terminator { Lexing.new_line lexbuf; comment_ml start lexbuf }
  | eof { Error("unterminated comment", start) }
  | _ { comment_ml start lexbuf }
