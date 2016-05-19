/* ocamlyacc parser for bean */
%{
open Bean_ast
%}

%token <bool> BOOL_CONST
%token <int> INT_CONST
%token <string> IDENT
%token BOOL INT
%token WRITE READ
%token ASSIGN
%token LPAREN RPAREN
%token EQ LT GT NE LE GE
%token PLUS MINUS MUL DIV
%token OR AND
%token NOT
%token SEMICOLON
%token EOF
%token LBRACE RBRACE
%token WHILE DO OD
%token IF ELSE THEN FI
%token PROC END
%token REF VAL
%token TYPEDEF
%token COLON PERIOD COMMA
%token <string> STRING_CONST

/* Precedences */
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ LT GT NE LE GE
%left PLUS MINUS
%left MUL DIV
%nonassoc UMINUS

%type <Bean_ast.program> program

%start program
%%

program:
  typedefs procs { { typedefs = List.rev $1; procs = List.rev $2} }
  
/* Builds typedefs in reverse order */
typedefs:
  | typedefs typedef { $2 :: $1 }
  | { [] }

typedef:
  TYPEDEF typespec IDENT { ( $3 , $2 ) }
  
typespec:
  | beantype { Ttype $1 }
  | LBRACE fielddefs RBRACE { Tfdef (List.rev $2) }
  | IDENT { Tid $1 }
  
beantype :
  | BOOL { Bool }
  | INT { Int }

/* Builds fielddefs in reverse order */
fielddefs:
  | fielddefs COMMA fielddef { $3 :: $1 }
  | fielddef { [$1] }
  
fielddef:
  IDENT COLON typespec { ($1, $3) }

/* Builds procs in reverse order */
procs:
  | procs proc { $2 :: $1 }
  | proc { [ $1 ] }
  
proc:
  PROC pheader pbody END { ( $2, $3 ) }
  
pheader:
  IDENT LPAREN params RPAREN { ($1, List.rev $3) }
  
/* Builds params in reverse order */
params:
  | params COMMA param { $3 :: $1 }
  | param { [$1] }
  | { [] }

param:
  indicator typespec IDENT { ($1, $2, $3) }
  
indicator:
  | REF { Ref }
  | VAL { Val }

pbody:
  decls stmts { { decls = List.rev $1; stmts = List.rev $2 } }

/* Builds decls in reverse order */
decls :
  | decls decl { $2 :: $1 }
  | { [] }  
  
decl :
  | typespec IDENT SEMICOLON { ($2, $1) }

stmtlist:
  | stmts { List.rev $1 }
  
/* Builds stmts in reverse order */
stmts:
  | stmts stmt { $2 :: $1 }
  | stmt { [$1] }

stmt:
  | atom_stmt SEMICOLON { $1 }
  | comp_stmt { $1 }
  
atom_stmt:
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | lvalue ASSIGN rvalue { Assign ($1, $3) }
  | IDENT LPAREN exprs RPAREN { Call ($1, List.rev $3) }

comp_stmt:
  | IF expr THEN stmtlist FI { If ($2, $4) }
  | IF expr THEN stmtlist ELSE stmtlist FI { Ifelse ($2, $4, $6) }
  | WHILE expr DO stmtlist OD {While ($2, $4) }
  
/* Builds exprs in reverse order */
exprs:
  |exprs COMMA expr { $3 :: $1 }
  |expr { [$1] }
  |{ [] }
  
rvalue :
  | expr { Rexpr $1 }
  | LBRACE fieldinits RBRACE { Rinits (List.rev $2) }
  
/* Builds fieldinits in reverse order */
fieldinits:
  | fieldinits COMMA fieldinit { $3 :: $1 }
  | fieldinit { [$1] }
  | { [] }
  
fieldinit:
  IDENT EQ rvalue { ($1, $3) }

lvalue:
  | IDENT { LId $1 }
  | lvalue PERIOD IDENT { LField ($1, $3) }

expr:
  | lvalue { Elval $1 }
  /* Constants */
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | STRING_CONST { Estring $1 }
  /* (expression) */
  | LPAREN expr RPAREN { $2 }
  /* Binary operators */
  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr DIV expr { Ebinop ($1, Op_div, $3) }
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | expr GT expr { Ebinop ($1, Op_gt, $3) }
  | expr NE expr { Ebinop ($1, Op_ne, $3) }
  | expr LE expr { Ebinop ($1, Op_le, $3) }
  | expr GE expr { Ebinop ($1, Op_ge, $3) }
  | expr OR expr { Ebinop ($1, Op_or, $3) }
  | expr AND expr { Ebinop ($1, Op_and, $3) }
  /* Unary operators */
  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | NOT expr { Eunop (Op_not, $2) }
  