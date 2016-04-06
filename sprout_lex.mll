{
open Sprout_parse

exception LexFail of string

let lex_fail lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let str =
    Format.sprintf "Illegal lexeme: line %d, col %d."
      (pos.Lexing.pos_lnum)
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) in
  raise (LexFail str)
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | digit
let digits = digit+
let ident = alpha alnum*
rule token = parse
    [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
  | '-'?['0'-'9']+ as lxm { INT_CONST(int_of_string lxm) }
  (* keywords *)
  | "bool" { BOOL }
  | "int" { INT }
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read" { READ }
  | "write" { WRITE }
  | ":=" { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '=' { EQ }
  | '<' { LT }
  | '>'	{ GT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | ';' { SEMICOLON }
  | ident as lxm { IDENT lxm }
  | eof { EOF }
  | _   { lex_fail lexbuf }
