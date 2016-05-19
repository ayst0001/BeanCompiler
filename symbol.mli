open Bean_ast
open Format

type stackPos = int

type varType = string

type tdef_detail = 
  | SingleType of varType
  | MultiType of (ident * varType) list

type ttypedef = (tdef_detail * (ident list)) 

type arg = (indicator * varType * ident * stackPos)

type variable = (varType * ident * stackPos)

type tproc = ((arg list) * (variable list) * ident)

type symbol_table = { ttypedefs: ttypedef list; tprocs : tproc list}

(*val print_oz_code : Format.formatter -> Bean_ast.t -> unit*)