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
  | If of expr * stmt * (expr * stmt) list * stmt
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

let append_nl s = s ^ "\n"

let rec string_of_stmt = function
    Expr(e) -> string_of_expr e ^ ";"
  | Return(e) -> (match e with
      Noexpr -> "return;"
    | expr   -> "return " ^ string_of_expr e ^ ";")
  | Block(stmts) ->
      "{\n" ^
       String.concat "" (List.map append_nl (List.map string_of_stmt stmts)) ^
      "}"
  | If(e, s, [], Block([])) ->
    "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s
  | If(e, s1, [], s2) ->
    "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "\n" ^
      "else\n" ^
      string_of_stmt s2
  | If (e, s, elifs, Block([])) -> 
    "if (" ^ string_of_expr e ^ ")\n" ^
    string_of_stmt s ^
    String.concat "" (List.map (fun elif -> "\nelif (" ^ string_of_expr (fst elif) ^ ")\n" ^
                                            string_of_stmt (snd elif)) elifs)
  | If (e, s1, elifs, s2) -> 
    "if (" ^ string_of_expr e ^ ")\n" ^
    string_of_stmt s1 ^
    String.concat "" (List.map (fun elif -> "\nelif (" ^ string_of_expr (fst elif) ^ ")\n" ^
                                             string_of_stmt (snd elif)) elifs) ^ "\n" ^
                     string_of_stmt s2
  | While(e, s) ->
      "while (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s
  | For(id, e1, e2, s) ->
      "for (" ^ id ^ " from " ^ string_of_expr e1 ^ " to" ^ string_of_expr e2 ^ ")\n" ^
      string_of_stmt s
                      

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";"

let string_of_fdecl fdecl =
  "fn " ^ fdecl.fname ^ "("  ^ String.concat ", " (List.map snd fdecl.formals) ^ ") -> " ^ string_of_typ fdecl.typ ^ "\n" ^
  "{\n" ^
  String.concat "" (List.map append_nl (List.map string_of_vdecl fdecl.locals)) ^
  String.concat "" (List.map append_nl (List.map string_of_stmt fdecl.body)) ^
  "}"

let string_of_program (vdecls, fdecls) =
  String.concat "" (List.map append_nl (List.map string_of_vdecl vdecls)) ^
  String.concat "" (List.map append_nl (List.map string_of_fdecl fdecls))
