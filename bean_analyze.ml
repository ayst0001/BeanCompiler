open Symbol
open Bean_ast
open Format

let stack_pos = ref 0

let rec gen_table fmt prog = 
  let templist = gen_procs fmt prog.procs in
  { ttypedefs = [] ; tprocs = templist }

and gen_procs fmt procs =
  match procs with
  | [] -> []
  | x :: xs -> (gen_proc fmt x) :: (gen_procs fmt xs)

and next_stack_pos = 
    stack_pos := !stack_pos + 1;
  
and gen_proc fmt (pheader, pbody) = 
  stack_pos := 0;
  (*fprintf fmt "this is proc_%s@," (first_element pheader);*)
  let args = gen_pheader fmt pheader in
    let variables = gen_decls fmt pbody.decls in
	  let name = first_element pheader in
	    (args, variables, name);
  (*Std.print (args, []);*)
      
and first_element (a,_) = a

and gen_tspec fmt = function
  | Ttype Bool -> "bool"
  | Ttype Int -> "int"
  | Tfdef fdefs -> "nothing"
  | Tid ident -> ident
  
and gen_pheader fmt (ident, params)=
  fprintf fmt "proc_%s:@," ident;
  gen_params fmt params;
  
and gen_params fmt params =
  match params with
  | [] -> []
  | [x] -> gen_param fmt x :: []
  | x :: xs -> (gen_param fmt x) :: (gen_params fmt xs)
  
(*not compatible with multi-type typedef yet, if needed, paramType have to be a list*)

and gen_param fmt (indicator, typespec, ident) = 
  next_stack_pos;
  (indicator, gen_tspec fmt typespec, ident, (!stack_pos - 1));
  
and gen_decls fmt decls = 
  match decls with
  | [] -> []
  | x :: xs -> (gen_decl fmt x) :: (gen_decls fmt xs)
  
and gen_decl fmt (ident, typespec) = 
  next_stack_pos;
  (gen_tspec fmt typespec, ident, (!stack_pos - 1));