(* Semantic checking for the Vowel compiler *)

open Ast
open Sast

module StringMap = Map.Make(String);;

module HashtblString =
   struct 
    type t = string
    let equal = ( = )
    let hash = Hashtbl.hash
   end;;

module StringHash = Hashtbl.Make(HashtblString);;

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (statements, globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  let check_bind tbl (t, n) =
    if t = Void then raise (Failure ("illegal void " ^ n)) else
    if (StringHash.mem tbl n) then raise (Failure ("duplicate " ^ n)) else 
      StringHash.add tbl n t; tbl
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float);
			                         ("printbig", Int);
                               ("printstr", String)]
  in
  let built_in_decls =
      StringMap.add "string_sub" {
      typ = Arr(String,99);
      fname = "string_sub";
      formals = [(String, "str"); (String, "str2")];
      locals = [];
      body = [] } built_in_decls
 in

  (* declare main function with all statements *)
  let main_decl = 
    {
      typ = Int;
      fname = "main";
      formals = [];
      locals = [];
      body = List.rev statements }
    in
  let functions = main_decl :: functions in

let built_in_decls =
  StringMap.add "slice" {
  typ = String;
  fname = "slice";
  formals = [(String, "str"); (Int, "from"); (Int, "to")];
  locals = [];
  body = [] } built_in_decls
  in

  let built_in_decls =
    StringMap.add "string_mult" {
    typ = String;
    fname = "string_mult";
    formals = [(String, "str"); (Int, "a")]; 
    locals = [];
    body = [] } built_in_decls
  in

let built_in_decls =
  StringMap.add "len"{
    typ = Int;
    fname = "len";
    formals = [(String, "str")];
    locals = [];
    body = []} built_in_decls 
  in
  

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let rec check_arr (arrtyp, lv) =
    (match arrtyp with
      Arr(ty,_) ->  check_arr (ty, lv+1)
    | _ -> (arrtyp, lv))

    in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;
    let tbl = StringHash.create 10 in
    let formal_tbl = StringHash.create 5 in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
        if lvaluet = rvaluet then lvaluet else
          (match lvaluet with
            (* Object(_) -> if rvaluet = Null then lvaluet else raise (Failure err) *)
          | Arr _ -> (match rvaluet with 
                          Arr _ -> let r_arr = check_arr (rvaluet, 0) in
                                    let l_arr = check_arr (lvaluet, 0) in
                                      if r_arr = l_arr then rvaluet else raise (Failure err)
                        | _ -> raise (Failure err))
          | _ -> raise (Failure err))
    in   

    (* Build local symbol table of variables for this function *)
    let _ = List.fold_left check_bind
      formal_tbl (func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringHash.find tbl s
      with Not_found -> 
        try StringHash.find formal_tbl s
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
(*

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
*)
    
    (* check if of array type, return element type *)
    let is_arr_ty (v, ty) = match ty with 
        Arr(t,_) -> 
          if t = Void then raise (Failure ("void type array " ^ v ^ " is not allowed")) 
          else t
      | _ -> raise (Failure ("cannot access an element in variable " ^ v ^ " of type " ^ string_of_typ ty))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal  l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | STRliteral l -> (String, SSTRliteral l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)

      | Assign(var, e) as ex -> 
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in let _ = (match lt with 
            Arr _ -> StringHash.replace tbl var lt
            | _ -> ignore 1)
          in (check_assign lt rt err, SAssign(var, (rt, e')))
        

      | DeclAssn(ty, var, e) as declassn ->
          (* raise (Failure ("I'm Being Called From DeclAssn"));*)
          ignore (check_bind tbl (ty, var));
          (* check_bind tbl (ty, var);    *)
          let (rt, e') = expr e in
          let _err = "illegal assignment " ^ string_of_typ ty ^ " = " ^ 
              string_of_typ rt ^ " in " ^ string_of_expr declassn 
          
          (*  update array size *)
          in let _ = (match ty with 
            Arr _ -> StringHash.replace tbl var ty
          | _ -> ignore 1)
          in (ty, SDeclAssn(ty, var, (ty, e')))

      | Increment(var, e) as ex -> 
          let l = Binop(Id(var), Add, e) in 
          let lt = type_of_identifier var
          and (rt, e') = expr l in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))

      | Decrement(var, e) as ex -> 
          let l = Binop(Id(var), Sub, e) in 
          let lt = type_of_identifier var
          and (rt, e') = expr l in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))


      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div | Mod when same && t1 = Float -> Float
          | Add  when same && t1 = String -> String
          | Sub when same && t1 = String -> Arr(String,99)
          | Equal | Neq            when same               -> Bool
          | Mult when t1 = String -> String
          | Intersec when same && t1 = String -> Arr(String, 99)
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
            | And | Or when same && t1 = Bool -> Bool
            | _ -> raise (Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))

      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, n) e = 
            let (et, e') = expr e 
            in let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in let _ = (check_assign ft et err, e')
            in let _ = (match et with 
              Arr _ -> StringHash.replace formal_tbl n et
              | _ -> ignore 1) in (et, e')
          in let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))

      
      | ArrayLit(el) as arraylit -> (* check if types of expr are consistent *)
          let ty_inconsistent_err = "inconsistent types in array " ^ string_of_expr arraylit in
          let fst_e = List.hd el in
          let (fst_ty, _) = expr fst_e in
          let (arr_ty_len, arr_ty_e) = List.fold_left (fun (t, l) e ->
              let (et, e') = expr e in
              let is_arr = (match et with
                  Arr _ -> true
                  | _ -> false) in if ((et = fst_ty) || is_arr) 
                                  then (t+1, (et, e')::l) 
                                  else (t, (et, e')::l)) (0,[]) el
          in if arr_ty_len != List.length el
            then raise (Failure ty_inconsistent_err)
            (* determine arr type *)
            else let arr_ty = Arr(fst_ty, arr_ty_len)
            in (arr_ty, SArrayLit(arr_ty_e))

      
      | ArrayAccess(v, e) as arrayacess ->(* check if type of e is an int *)
          let (t, e') = expr e in
          if t != Int then raise 
          (Failure (string_of_expr e ^ " is not of int → type in " ^ string_of_expr arrayacess)) else
            (* check if variable is array type *)
          let v_ty = type_of_identifier v in
          let e_ty = is_arr_ty (v, v_ty) 
        in (e_ty, SArrayAccess(v, (t, e')))
      | ArrAssign(v, e1, e2) as arrassign ->(* check if type of e is an int *)
          let (t, e1') = expr e1 in
          if t != Int then raise (Failure (string_of_expr e1 ^ " is not of int type → in " ^ string_of_expr arrassign))
          else (* check if variable is array type *) 
          let v_ty = type_of_identifier v in
          let e_ty = is_arr_ty (v, v_ty) in
          let (rt, e2') = expr e2 in
          (e_ty, SArrAssign(v, (t,e1'), (rt,e2')))
          
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	      SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> let c = check_stmt s in
                                  c :: check_stmt_list ss
                                  (* check_stmt s :: check_stmt_list ss *)
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
