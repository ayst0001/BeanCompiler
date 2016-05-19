(* Specification of an AST for bean *)
type ident = string
 
(* primitive type in bean *)
type beantype =
  | Bool
  | Int

type typespec = 
  | Ttype of beantype
  | Tfdef of (ident * typespec) list
  | Tid of ident
  
type typedef = (ident * typespec)

type indicator = 
  | Val
  | Ref
  
type param = (indicator * typespec * ident)
  
type pheader = ( ident * ( param list ) )

type decl = (ident * typespec)

type lvalue =
  | LId of ident
  | LField of (lvalue * ident)

type binop =
  | Op_add | Op_sub | Op_mul | Op_div | Op_eq | Op_lt | Op_gt | Op_ne | Op_le | Op_ge | Op_or |Op_and

type unop =
  | Op_minus | Op_not

type expr =
  | Elval of lvalue
  (* Constants *)
  | Ebool of bool
  | Eint of int
  | Estring of string
  (* Operations *)
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)

type rvalue =
  | Rexpr of expr
  | Rinits of (ident * rvalue) list

(* Statements included atomic and composite *)
type stmt = 
  (* Atomic statements *)
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | Call of (ident * (expr list))
  (* Composite statements *)
  | If of (expr * (stmt list))
  | Ifelse of (expr * (stmt list) * (stmt list))
  | While of (expr * (stmt list))
  
type pbody = { 
  decls: decl list;
  stmts: stmt list
}

type proc = ( pheader * pbody )

type program = {
  typedefs : typedef list;
  procs: proc list
}

type t = program
