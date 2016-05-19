open Bean_ast
open Format
open Symbol

let pheader_var_num = ref 0

let pbody_var_num = ref 0

let stack_pos = ref 0

let rec print_oz_code fmt symbol_table prog = 
  (*print_typedefs fmt prog.typedefs;*)
  print_oz_head fmt;
  fprintf fmt "procs.length: %d@," (List.length symbol_table.tprocs);
  fprintf fmt "1st procs'args length: %d@," (List.length (first_element (List.nth symbol_table.tprocs 1)));
  (*print_procs fmt prog.procs*)

and print_oz_head fmt = 
   fprintf fmt "call proc_main@,halt@,";
   
and first_element (a,_,_) = a
(*
   
(*and print_typedefs fmt = function
  | [] -> ()
  | x :: xs -> print_typedef fmt x; print_typedefs fmt xs
 
and print_typedef fmt (ident, typespec) = 
  (* each typedef is in a single box, and is printed on one line *)
  fprintf fmt "@[typedef@ %a@ %s@]@." print_tspec typespec ident*)
  
and print_tspec fmt = function
  | Ttype Bool -> "bool"
  | Ttype Int -> "int"
  | Tfdef fdefs -> "nothing" (*fprintf fmt "{%a}" print_fdefs fdefs*)
  | Tid ident -> ident
  
(*and print_fdefs fmt = function
  | [] -> ()
  | [x] -> print_fdef fmt x
  | x :: xs -> fprintf fmt "%a,@ " print_fdef x; print_fdefs fmt xs
  
and print_fdef fmt (ident, typespec) =
  fprintf fmt "%s@ :@ %a" ident print_tspec typespec *)

and print_procs fmt = function
  | [] -> ()
  | x :: xs -> print_proc fmt x; print_procs fmt xs

and print_proc fmt (pheader, pbody) = 
  (* separate procs by one blank line *)
  print_newline ();
  fprintf fmt "#prologue:@,";
  !proc_args := [];
  !stack_pos := 0;
  let variables = print_decls fmt pbody.decls in
    let params = second_element pheader in
	  let num = look_up_param_num pbody.decls params in 
        let args = print_pheader fmt pheader num in
	      prog_symbol_table.tprocs @ [(args, variables)];
  Std.print prog_symbol_table.tprocs;
  print_stmts fmt pbody.stmts
  
and look_up_param_num l1 l2 = (List.length l1) + (List.length l2)

and second_element (_,a) = a
  
and next_stack_pos = 
  !stack_pos = !stackPos + 1
  
and print_pheader fmt (ident, params) num =  (*怎么写？怎么返回proc_params list？*)
  fprintf fmt "proc_%s:@,push_stack_frame %d@," ident num;
  ( print_params fmt params, ident ) 
  
and print_params fmt = function
  | [] -> !proc_params
  | [x] -> print_param fmt x
  | x :: xs -> print_param fmt x; print_params fmt xs
  
and print_param fmt (indicator, typespec, ident) = (*not compatible with multi-type typedef yet*)
  let paramType = print_tspec fmt typespec in
  !proc_args :: (indicator, paramType, ident, next_stack_pos);
  (match indicator with
  | Val -> fprintf fmt "store 0 r0 %s@," ident
  | Ref -> fprintf fmt "store_indirect 0 r0 %s@," ident)

(*and print_pbody fmt pbody = 
  print_decls fmt pbody.decls;
  (* separate declarations and statements by one blank line *)
  (* since pbody is in vertical box, break hint leads to line break *)
  fprintf fmt "@,";
  print_stmts fmt pbody.stmts;*)
  
and print_decls fmt = []
(*function
  | [] -> ()
  | x :: xs -> print_decl fmt x; print_decls fmt xs
  
and print_decl fmt (ident, typespec) = 
  (* each declaration is on a separa line *)
  fprintf fmt "%a %s;@," print_tspec typespec ident
  
and print_stmts fmt = ()*)
(*function
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_stmt x
  (* each statement starts on a new line *)
  | x :: xs -> fprintf fmt "%a@,%a" print_stmt x print_stmts xs
  
and print_stmt fmt = function
  | Assign (lvalue, rvalue) -> fprintf fmt "%a := %a;" 
      print_lvalue lvalue print_rvalue rvalue
  | Read lvalue -> fprintf fmt "read %a;" print_lvalue lvalue
  | Write expr -> fprintf fmt "write %a;" print_expr_p expr
  | Call (ident, exprs) -> fprintf fmt "%s(%a);" ident print_exprs exprs
  (* statements inside a conditinal or while loop are indented 4 spaces *)
  (* while...do and if...then print on one line *)
  (* od indented as the corresponding while and same to if..then..else *)
  (* since they are all in the same vertical box *)
  | If (expr, stmts) -> fprintf fmt "if %a then@;<0 4>@[<v>%a@]@,fi" 
      print_expr_p expr print_stmts stmts
  | Ifelse (expr, stmts_a, stmts_b) -> fprintf fmt 
      "if %a then@;<0 4>@[<v>%a@]@,else@;<0 4>@[<v>%a@]@,fi" 
	  print_expr_p expr print_stmts stmts_a print_stmts stmts_b
  | While (expr, stmts) -> fprintf fmt "while %a do@;<0 4>@[<v>%a@]@,od" 
      print_expr_p expr print_stmts stmts

(* modified for grammar regulation of fprintf *)	  
and print_expr_p fmt expr = print_expr fmt 0 expr
  
and print_lvalue fmt = function
  | LId ident -> fprintf fmt "%s" ident
  | LField (lvalue, ident) -> fprintf fmt "%a.%s" print_lvalue lvalue ident
  
and print_rvalue fmt = function
  | Rexpr expr -> print_expr fmt 0 expr
  | Rinits inits -> fprintf fmt "{%a}" print_inits inits
  
and print_inits fmt = function
  | [] -> ()
  | [x] -> print_init fmt x
  | x :: xs -> fprintf fmt "%a, " print_init x; print_inits fmt xs
  
and print_init fmt (ident, rvalue) = 
  fprintf fmt "%s = %a" ident print_rvalue rvalue
  
and print_exprs fmt = function
  | [] -> ()
  | [x] -> print_expr fmt 0 x
  | x :: xs -> fprintf fmt "%a, " print_expr_p x; print_exprs fmt xs
  
(* prec = precedence *)
and print_expr fmt prec = function
  | Ebool b -> 
    begin match b with
	| true -> fprintf fmt "%s" "true"
	| false -> fprintf fmt "%s" "false"
	end
  | Eint i -> fprintf fmt "%d" i
  | Estring str -> fprintf fmt "%s" str
  | Elval lvalue -> print_lvalue fmt lvalue
  | Ebinop (expr1, binop, expr2) -> print_binop fmt expr1 binop expr2 prec
  | Eunop (unop, expr) -> 
    begin match unop with
	| Op_minus -> 
	  begin 
	  open_paren fmt prec 12;
	  fprintf fmt "-";
	  print_expr fmt 12 expr;
	  close_paren fmt prec 12;
	  end
	| Op_not -> 
	  begin 
	  open_paren fmt prec 4;
	  fprintf fmt "not ";
	  print_expr fmt 4 expr;
	  close_paren fmt prec 4;
	  end
	end

(* classify different precedences of operators *)
and print_binop fmt expr1 binop expr2 prec =
  match binop with 
  | Op_add | Op_sub -> print_eop fmt expr1 expr2 prec 8 1 binop
  | Op_mul | Op_div -> print_eop fmt expr1 expr2 prec 10 1 binop
  | Op_eq | Op_lt | Op_gt | Op_ne | Op_le | Op_ge -> print_eop fmt
    expr1 expr2 prec 6 0 binop
  | Op_or -> print_eop fmt expr1 expr2 prec 0 1 binop
  | Op_and -> print_eop fmt expr1 expr2 prec 2 1 binop
  
(* processing parentheses according to operator precedences *)
(* prec => the top level precedence *)
(* op_prec => current operator precedence *)
(* isleft => 1 if the operator is left-associative; 0 for non-associative *)
and print_eop fmt expr1 expr2 prec op_prec isleft binop = 
  open_paren fmt prec op_prec;
  print_expr fmt op_prec expr1;
  ( match binop with
  | Op_add -> fprintf fmt " + "
  | Op_sub -> fprintf fmt " - "
  | Op_mul -> fprintf fmt " * "
  | Op_div -> fprintf fmt " / "
  | Op_eq -> fprintf fmt " = "
  | Op_lt -> fprintf fmt " < "
  | Op_gt -> fprintf fmt " > "
  | Op_ne -> fprintf fmt " != "
  | Op_le -> fprintf fmt " <= "
  | Op_ge -> fprintf fmt " >= "
  | Op_or -> fprintf fmt " or "
  | Op_and -> fprintf fmt " and "
  );
  print_expr fmt (op_prec + isleft) expr2;
  close_paren fmt prec op_prec;
  
and open_paren fmt cur_prec op_prec =
  if cur_prec > op_prec then 
    fprintf fmt "("

and close_paren fmt cur_prec op_prec =
  if cur_prec > op_prec then 
    fprintf fmt ")"*)
	
	
	*)