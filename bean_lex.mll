{
open Bean_parse
}

let alpha_ = ['a' - 'z' 'A' - 'Z' '_']
let valid_char = alpha_ | '''
let ident = alpha_ valid_char*
let estring = '"'[^'\t' '\n' '"']*'"'

rule token = parse
  (* skip blanks *)
    [' ' '\t']    { token lexbuf }
	
  (* skip comments *)
  | '#'[^'\n']*  { token lexbuf }
  
  | '\n'          { Lexing.new_line lexbuf ; token lexbuf }
  | '-'?['0'-'9']+ as lxm { INT_CONST(int_of_string lxm) }
  
  (* keywords *)
  | "bool" { BOOL }
  | "int" { INT }
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "read" { READ }
  | "write" { WRITE }
  | "or" { OR }
  | "and" { AND }
  | "not" { NOT }
  | "typedef" { TYPEDEF }
  | "proc" { PROC }
  | "end" { END }
  | "while" { WHILE }
  | "do" { DO }
  | "od" { OD }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }
  | "val" { VAL }
  | "ref" { REF }
  
  (* operators *)
  | ":=" { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '=' { EQ }
  | '<' { LT }
  | '>'	{ GT }
  | "!="	{ NE }
  | "<="	{ LE }
  | ">="	{ GE }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  
  (* symbols *)
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '.' { PERIOD }
  | ',' { COMMA }
  
  | ident as lxm { IDENT lxm }
  | estring as lxm { STRING_CONST lxm }
  | eof { EOF }
