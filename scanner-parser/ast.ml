type typ = Int | Bool | Float | Str | Void

type bind = typ * string

type expr =
    Iliteral of int
  | Fliteral of float
  | Bliteral of bool
  | Sliteral of string
  | Id of string
  | Assign of string * expr

type stmt =
    Expr of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list; 
    locals : bind list;
    body : stmt list }

type program = bind list * func_decl list

let string_of_typ = function
    Int   -> "int"
  | Bool  -> "bool"
  | Float -> "float"
  | Str   -> "str"
  | Void  -> "void"

let rec string_of_expr = function
    Iliteral(x) -> string_of_int x
  | Fliteral(x) -> string_of_float x
  | Bliteral(b) -> string_of_bool b
  | Sliteral(s) -> s
  | Id(id) -> id
  | Assign(id, e) -> id ^ " = " ^ string_of_expr e

let rec string_of_stmt = function
    Expr(e) -> string_of_expr e ^ ";\n"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "fn " ^ fdecl.fname ^ "("  ^ String.concat ", " (List.map snd fdecl.formals) ^ ") -> " ^ string_of_typ fdecl.typ ^ "\n" ^
  "{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vdecls, fdecls) =
  String.concat "" (List.map string_of_vdecl vdecls) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl fdecls)
