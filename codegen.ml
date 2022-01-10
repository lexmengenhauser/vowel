(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String);;

module HashtblString =
   struct 
    type t = string
    let equal = ( = )
    let hash = Hashtbl.hash
   end;;

module StringHash = Hashtbl.Make(HashtblString);;

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and str_t      = L.pointer_type (L.i8_type context)
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context in

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> str_t
    (* | A.Arr(ty,_) -> L.pointer_type (ltype_of_typ ty) *)
    | A.Arr(ty,_) -> L.pointer_type (ltype_of_typ ty)
  in

  (* Create a map of global variables after creating each *)
  (* let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in *)

    
  let string_concat_t : L.lltype =
    L.function_type str_t [| str_t; str_t |] in  
  let string_concat_f : L.llvalue =
    L.declare_function "string_concat" string_concat_t the_module in
  
  let string_inequality_t : L.lltype = 
    L.function_type i32_t [| str_t; str_t |] in
  let string_inequality_f : L.llvalue =
    L.declare_function "string_inequality" string_inequality_t the_module in

  let string_intersection_t : L.lltype = 
      L.function_type (L.pointer_type str_t) [| str_t; str_t |] in
  let string_intersection_f : L.llvalue =
      L.declare_function "string_intersection" string_intersection_t the_module in
  let string_sub_t : L.lltype = 
    L.function_type (L.pointer_type str_t) [| str_t; str_t |] in
  let string_sub_f : L.llvalue =
    L.declare_function "string_sub" string_sub_t the_module in

let slice_t : L.lltype =
    L.function_type str_t [| str_t; i32_t; i32_t |] in  
  let slice_f : L.llvalue =
    L.declare_function "slice" slice_t the_module in

  let len_t : L.lltype =
    L.function_type i32_t [| str_t |] in  
  let len_f : L.llvalue =
    L.declare_function "len" len_t the_module in
  
    let string_mult_t : L.lltype = 
      L.function_type str_t [| str_t; i32_t |] in
    let string_mult_f : L.llvalue =
      L.declare_function "string_mult" string_mult_t the_module in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in
  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Adding StringHash tbl here -- TODO: add all the formals? See JavaLite  *)
    (* let tbl = StringHash.create 10
  in *)
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder 
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    (* let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m  *)

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      (* and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in *)

    (* Construct a hash table for function formals and locals
       add all the formals first *)
    let tbl = StringHash.create 10 in
    let formal_tbl = StringHash.create 5 in
      let add_formal tbl (t, n) p = 
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
            ignore (L.build_store p local builder);
        StringHash.add tbl n local; tbl in
      let _ = List.fold_left2 add_formal formal_tbl fdecl.sformals
        (Array.to_list (L.params the_function)) in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringHash.find tbl n(*StringMap.find tbl n n local_vars*)
                     with Not_found -> try 
                     StringHash.find formal_tbl n(*StringMap.find formal_tbl n n global_vars*)
                   with Not_found -> raise (Failure ("variable " ^ n ^ " not found in lookup"))
    in
    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	      SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SSTRliteral s -> L.build_global_stringptr s "string" builder
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SArrayLit arr -> let len = L.const_int i32_t (List.length arr) in
            let size = L.const_int i32_t ((List.length arr) + 1) in
            let (fst_t, _) = List.hd arr in
            let ty = ltype_of_typ (A.Arr(fst_t, (List.length arr))) in
            (* allocate memory for array *)
            let arr_alloca = L.build_array_alloca ty size "arr" builder in
            (* bitcast *)
            let arr_ptr = L.build_pointercast arr_alloca ty "arrptr" builder in
            (* store all elements *)
            let elts = List.map (expr builder) arr in
            let store_elt ind elt =
            let pos = L.const_int i32_t (ind) in
            let elt_ptr = L.build_gep arr_ptr [| pos |] "arrelt" builder in
            ignore(L.build_store elt elt_ptr builder)
            in List.iteri store_elt elts;
            let elt_ptr = L.build_gep arr_ptr [| len |] "arrlast" builder in
            let null_elt = L.const_null (L.element_type ty) in
            ignore(L.build_store null_elt elt_ptr builder);
            arr_ptr
      | SArrayAccess (s, e) ->
            let ind = expr builder e in
            let (ty, _) = e in
            (* increment index by one to get actual ptr position *)
            let pos = L.build_add ind (L.const_int i32_t 0) "accpos" builder in
            let arr = expr builder (ty, (SId s)) in
            let elt = L.build_gep arr [| pos |] "acceltptr" builder in
            L.build_load elt "accelt" builder
      | SArrAssign (s, e1, e2) ->
            let ind = expr builder e1 in
            let (ty, _) = e1 in
            (* increment index by one to get actual ptr position *)
            let pos = L.build_add ind (L.const_int i32_t 0) "accpos" builder in
            let arr = expr builder (ty, (SId s)) in
            let new_val = expr builder e2 in
            let elt_ptr = L.build_gep arr [| pos |] "arrelt" builder in
            L.build_store new_val elt_ptr builder
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SDeclAssn (t, s, e) -> 
        let e' = expr builder e in
        let var = L.build_alloca (ltype_of_typ t) s builder in
            ignore (L.build_store e' var builder);
        StringHash.add tbl s var; e'

      | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with 
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv 
          | A.Mod     -> L.build_frem
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.Intersec -> raise (Failure "internal error: semant should have rejected intersection binop with floats")
          | A.And | A.Or ->
	          raise (Failure "internal error: semant should have rejected and/or on float")
	      ) e1' e2' "tmp" builder

      | SBinop ((A.String,_ ) as e1, op, e2) -> 
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
            A.Add     -> L.build_call string_concat_f [| e1'; e2' |] "string_concat" builder
          | A.Sub     -> L.build_call string_sub_f [| e1'; e2' |] "string_sub" builder
          | A.Equal ->  (L.build_icmp L.Icmp.Eq) (L.const_int i32_t 0) (L.build_call string_inequality_f [| e1'; e2' |] "string_inequality" builder) "tmp" builder
          | A.Neq     -> (L.build_icmp L.Icmp.Ne) (L.const_int i32_t 0) (L.build_call string_inequality_f [| e1';e2' |] "string_inequality" builder) "tmp" builder
          | A.Intersec -> L.build_call string_intersection_f [| e1'; e2' |] "string_intersection" builder
          | A.Mult ->  L.build_call string_mult_f [| e1'; e2' |] "string_mult" builder
          | _ -> raise (Failure ("operation " ^ (A.string_of_op op) ^ " not implemented")))


      | SBinop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | A.Intersec -> raise (Failure "internal error: intersection operator should have triggered on binop with string")
        ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in
        (match op with
          A.Neg when t = A.Float   -> L.build_fneg 
          | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder


      | SCall ("print", [e]) | SCall ("printb", [e]) -> L.build_call printf_func [| int_format_str ; (expr builder e) |] "printf" builder
      | SCall ("printbig", [e]) -> L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) -> L.build_call printf_func [| float_format_str ; (expr builder e) |] "printf" builder
      | SCall ("printstr", [e]) -> L.build_call printf_func [| string_format_str ; (expr builder e) |] "printf" builder
      | SCall ("slice", [v;e1;e2]) -> L.build_call slice_f [| (expr builder v); (expr builder e1); (expr builder e2) |] "slice" builder
      | SCall ("len", [e]) -> L.build_call len_f [| (expr builder e) |] "len" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
