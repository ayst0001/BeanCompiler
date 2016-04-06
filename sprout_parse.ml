type token =
  | BOOL_CONST of (bool)
  | INT_CONST of (int)
  | IDENT of (string)
  | BOOL
  | INT
  | WRITE
  | READ
  | ASSIGN
  | LPAREN
  | RPAREN
  | EQ
  | LT
  | GT
  | PLUS
  | MINUS
  | MUL
  | SEMICOLON
  | EOF

open Parsing;;
# 3 "sprout_parse.mly"
open Sprout_ast
# 25 "sprout_parse.ml"
let yytransl_const = [|
  260 (* BOOL *);
  261 (* INT *);
  262 (* WRITE *);
  263 (* READ *);
  264 (* ASSIGN *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* EQ *);
  268 (* LT *);
  269 (* GT *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* MUL *);
  273 (* SEMICOLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* BOOL_CONST *);
  258 (* INT_CONST *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\004\000\002\000\002\000\005\000\005\000\003\000\003\000\
\006\000\007\000\007\000\007\000\010\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\000\000"

let yylen = "\002\000\
\002\000\003\000\002\000\000\000\001\000\001\000\002\000\000\000\
\002\000\002\000\002\000\003\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\026\000\000\000\005\000\006\000\000\000\003\000\
\000\000\014\000\000\000\000\000\007\000\000\000\000\000\000\000\
\015\000\016\000\000\000\000\000\017\000\000\000\010\000\009\000\
\000\000\002\000\000\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\025\000\000\000\000\000\000\000\
\000\000\000\000\020\000"

let yydgoto = "\002\000\
\003\000\004\000\007\000\008\000\009\000\013\000\014\000\021\000\
\022\000\036\000"

let yysindex = "\005\000\
\000\000\000\000\000\000\058\255\000\000\000\000\024\255\000\000\
\006\255\000\000\017\255\022\255\000\000\012\255\056\255\030\255\
\000\000\000\000\017\255\017\255\000\000\045\255\000\000\000\000\
\017\255\000\000\039\255\000\000\017\255\017\255\017\255\017\255\
\017\255\017\255\045\255\000\000\000\000\008\255\008\255\008\255\
\049\255\049\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\051\255\000\000\000\000\246\254\250\254\011\255\
\023\255\031\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\254\255\
\239\255\000\000"

let yytablesize = 264
let yytable = "\021\000\
\008\000\027\000\028\000\022\000\015\000\001\000\021\000\035\000\
\016\000\023\000\022\000\038\000\039\000\040\000\041\000\042\000\
\043\000\017\000\018\000\010\000\023\000\032\000\033\000\034\000\
\010\000\019\000\010\000\023\000\024\000\011\000\012\000\020\000\
\018\000\018\000\018\000\018\000\018\000\018\000\001\000\018\000\
\019\000\019\000\019\000\019\000\019\000\019\000\026\000\019\000\
\037\000\029\000\030\000\031\000\032\000\033\000\034\000\029\000\
\030\000\031\000\032\000\033\000\034\000\005\000\006\000\025\000\
\034\000\000\000\011\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\008\000\008\000"

let yycheck = "\010\001\
\000\000\019\000\020\000\010\001\007\000\001\000\017\001\025\000\
\003\001\012\000\017\001\029\000\030\000\031\000\032\000\033\000\
\034\000\001\001\002\001\003\001\010\001\014\001\015\001\016\001\
\003\001\009\001\003\001\017\001\017\001\006\001\007\001\015\001\
\010\001\011\001\012\001\013\001\014\001\015\001\000\000\017\001\
\010\001\011\001\012\001\013\001\014\001\015\001\017\001\017\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\011\001\
\012\001\013\001\014\001\015\001\016\001\004\001\005\001\008\001\
\016\001\255\255\017\001\017\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\255\255\006\001\007\001"

let yynames_const = "\
  BOOL\000\
  INT\000\
  WRITE\000\
  READ\000\
  ASSIGN\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  LT\000\
  GT\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  SEMICOLON\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL_CONST\000\
  INT_CONST\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 29 "sprout_parse.mly"
              ( { decls = List.rev _1 ; stmts = List.rev _2 } )
# 197 "sprout_parse.ml"
               : Sprout_ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typespec) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 32 "sprout_parse.mly"
                             ( (_2, _1) )
# 205 "sprout_parse.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 35 "sprout_parse.mly"
               ( _2 :: _1 )
# 213 "sprout_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "sprout_parse.mly"
    ( [] )
# 219 "sprout_parse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "sprout_parse.mly"
         ( Bool )
# 225 "sprout_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "sprout_parse.mly"
        ( Int )
# 231 "sprout_parse.ml"
               : 'typespec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 44 "sprout_parse.mly"
               ( _2 :: _1 )
# 239 "sprout_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "sprout_parse.mly"
    ( [] )
# 245 "sprout_parse.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_body) in
    Obj.repr(
# 48 "sprout_parse.mly"
                      ( _1 )
# 252 "sprout_parse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 51 "sprout_parse.mly"
                ( Read _2 )
# 259 "sprout_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "sprout_parse.mly"
               ( Write _2 )
# 266 "sprout_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rvalue) in
    Obj.repr(
# 53 "sprout_parse.mly"
                         ( Assign (_1, _3) )
# 274 "sprout_parse.ml"
               : 'stmt_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "sprout_parse.mly"
         ( Rexpr _1 )
# 281 "sprout_parse.ml"
               : 'rvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "sprout_parse.mly"
          ( LId _1 )
# 288 "sprout_parse.ml"
               : 'lvalue))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 62 "sprout_parse.mly"
               ( Ebool _1 )
# 295 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "sprout_parse.mly"
              ( Eint _1 )
# 302 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lvalue) in
    Obj.repr(
# 64 "sprout_parse.mly"
           ( Elval _1 )
# 309 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "sprout_parse.mly"
                   ( Ebinop (_1, Op_add, _3) )
# 317 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "sprout_parse.mly"
                    ( Ebinop (_1, Op_sub, _3) )
# 325 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "sprout_parse.mly"
                  ( Ebinop (_1, Op_mul, _3) )
# 333 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "sprout_parse.mly"
                 ( Ebinop (_1, Op_eq, _3) )
# 341 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "sprout_parse.mly"
                 ( Ebinop (_1, Op_lt, _3) )
# 349 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "sprout_parse.mly"
                 ( Ebinop (_1, Op_gt, _3) )
# 357 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "sprout_parse.mly"
                            ( Eunop (Op_minus, _2) )
# 364 "sprout_parse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "sprout_parse.mly"
                       ( _2 )
# 371 "sprout_parse.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Sprout_ast.program)
