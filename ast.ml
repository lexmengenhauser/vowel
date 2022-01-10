(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Intersec

type uop = Neg | Not

type typ = Int | Bool | Float | Void | String | Arr of (typ * int)

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | STRliteral of string
  | BoolLit of bool
  | Id of string 
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | DeclAssn of typ * string * expr
  | Increment of string * expr 
  | Decrement of string * expr
  | Call of string * expr list
  | ArrayAccess of string * expr
  | ArrayLit of expr list
  | ArrAssign of string * expr * expr
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = stmt list * bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Intersec -> "&"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
  Int -> "int"
| String -> "string"
| Bool -> "bool"
| Float -> "float"
| Void -> "void"
(* | Arr(t, _) -> string_of_typ t ^ "[]" *)
| Arr(t, _) -> string_of_typ t ^ "[]"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | STRliteral(s) -> s
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | DeclAssn(t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e 
  | Increment(v, e) -> v ^ "+= " ^ string_of_expr e
  | Decrement(v, e) -> v ^ "-="  ^ string_of_expr e 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrayAccess (s, e) ->  s ^ "[" ^ string_of_expr e ^ "]"
  | ArrayLit(e) -> "[" ^ String.concat "," (List.map string_of_expr (List.rev e)) ^ "]"
  | ArrAssign(s, e1, e2) -> s ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr  e2
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, vars, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^
  String.concat "\n" (List.map string_of_vdecl vars) ^ "\n" ^ 
  String.concat "\n" (List.map string_of_fdecl funcs)
