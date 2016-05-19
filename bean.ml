module P = Bean_parse

(* type definition *)
type compiler_mode = PrettyPrint | Compile

(* variable declarations *)
let infile_name = ref None
let mode = ref Compile

(* print current position of lexbuf *)
let err_pos lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
    Format.sprintf ": line %d, col %d."
      (pos.Lexing.pos_lnum)
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) 

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)

(* define spec list *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode"
  ]

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;
  (* Open the input file *)
  let infile = match !infile_name with
  | None -> stdin
  | Some fname -> open_in fname in
  (* Initialize lexing buffer *)
  let lexbuf = Lexing.from_channel infile in
  (* Call the parser *)
  try
    let prog = Bean_parse.program Bean_lex.token lexbuf in
    match !mode with
    | PrettyPrint ->
      Bean_pprint.print_program Format.std_formatter prog 
    | Compile -> let symbol_table = Bean_analyze.gen_table Format.std_formatter prog in
	               Codegen.print_oz_code Format.std_formatter symbol_table prog 
  with
  (* Error handling *)
    | Failure x -> print_string ("Lexing Error" ^ (err_pos lexbuf) ^ "\n")
	| Parsing.Parse_error -> print_string 
	("Parsing Error" ^ (err_pos lexbuf) ^ "\n")

let _ = main ()
