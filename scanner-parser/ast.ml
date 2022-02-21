type binop =
    Add
  | Sub
  | Mlt
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | Xor
  | And
  | Or

type unop =
    Not
  | Neg

type typ =
    Int
  | Bool
  | Float
  | Str
  | Void

type bind = typ * string

type expr =
    Iliteral of int
  | Fliteral of float
  | Bliteral of bool
  | Sliteral of string
  | Id of string
  | Assign of string * expr
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Expr of expr
  | Return of expr
  | Block of stmt list
  | If of expr * stmt
  | For of string * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list; 
    locals : bind list;
    body : stmt list }

type program = bind list * func_decl list

let string_of_binop = function
    Add -> "+"
  | Sub -> "-"
  | Mlt -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq  -> "=="
  | Neq -> "!="
  | Lt  -> "<"
  | Leq -> "<="
  | Gt  -> ">"
  | Geq -> ">="
  | Xor -> "xor"
  | And -> "and"
  | Or  -> "or"

let string_of_unop = function
    Not -> "not"
  | Neg -> "-"

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
  | Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | Unop(op, e) -> (match op with
      Not -> string_of_unop op ^ " (" ^ string_of_expr e ^ ")"
    | Neg -> string_of_unop op ^ "(" ^ string_of_expr e ^ ")")
  | Call(f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Expr(e) -> string_of_expr e ^ ";\n"
  | Return(e) -> (match e with
      Noexpr -> "return;\n"
    | expr   -> "return " ^ string_of_expr e ^ ";\n")
  | Block(stmts) ->
      "{\n" ^
       String.concat "" (List.map string_of_stmt stmts) ^
      "}\n"
  | If (e, s) ->
      "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s
  | While(e, s) ->
      "while (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s
  | For(id, e1, e2, s) ->
      "for (" ^ id ^ " from " ^ string_of_expr e1 ^ " to" ^ string_of_expr e2 ^ ")\n" ^
      string_of_stmt s
                      

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
