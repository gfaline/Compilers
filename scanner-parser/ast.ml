let fst_trpl (a, _, _) = a
let snd_trpl (_, b, _) = b
let trd_trpl (_, _, c) = c

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
  | Obj
  | List of typ

type bind = typ * string

type obj_decl = {
  oname : string;
  props : bind list }

type expr =
  (* literals *)
    Iliteral of int
  | Fliteral of float
  | Bliteral of bool
  | Sliteral of string
  | Lliteral of expr list
  (* function call *)
  | Call of string * expr list
  (* assignment *)
  | Assign of string * expr
  | Setprop of string * string * expr
  (* ID evaluation*)
  | Id of string
  | Getprop of string * string
  (* list indexing *)
  | Index of string * expr
  (* operators *)
  | Binop of expr * binop * expr
  | Unop of unop * expr
  (* other *)
  | Parentheses of expr
  | Noexpr

type stmt =
    Expr of expr
  | Return of expr
  | If of expr * stmt list * (expr * stmt list) list * stmt list
  | For of string * expr * expr * stmt list
  | While of expr * stmt list
  | Break
  | Continue
  | Bind of string * string * string
  | Unbind of string * string * string

type func_decl = {
  typ : typ;
  fname : string;
  formals : bind list; 
  locals : bind list;
  body : stmt list }

type program = bind list * obj_decl list * func_decl list

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

let rec string_of_typ = function
    Int   -> "int"
  | Bool  -> "bool"
  | Float -> "float"
  | Str   -> "str"
  | Void  -> "void"
  | Obj   -> "obj"
  | List(t) -> string_of_typ t ^ " list"

let rec string_of_expr = function
  (* literals *)
    Iliteral(x) -> string_of_int x
  | Fliteral(x) -> string_of_float x
  | Bliteral(b) -> string_of_bool b
  | Sliteral(s) -> s
  | Lliteral(es) -> "[" ^ String.concat ", " (List.map string_of_expr es) ^ "]"
  (* function call *)
  | Call(f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  (* assignment *)
  | Assign(id, e) -> id ^ " = " ^ string_of_expr e
  | Setprop(o, p, e) -> o ^ "." ^ p ^ " = " ^ string_of_expr e
  (* ID evaluation *)
  | Id(id) -> id
  | Getprop(o, p) -> o ^ "." ^ p
  (* list indexing *)
  | Index(id, e) -> id ^ "[" ^ string_of_expr e ^ "]"
  (* operators*)
  | Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | Unop(op, e) -> (match op with
      Not -> string_of_unop op ^ " (" ^ string_of_expr e ^ ")"
    | Neg -> string_of_unop op ^ "(" ^ string_of_expr e ^ ")")
  (* other *)
  | Parentheses(e) -> "(" ^ string_of_expr e ^ ")"
  | Noexpr -> ""

let rec string_of_stmt_list = function
    [] -> ""
  | st::sts -> (match st with
      Expr(e) -> string_of_expr e ^ ";"
    | Return(e) -> (match e with
        Noexpr -> "return;"
      | expr   -> "return " ^ string_of_expr e ^ ";")
    | If(e, s, [], []) ->
      "if " ^ string_of_expr e ^ "\n" ^
      "{\n" ^
      string_of_stmt_list s ^
      "}"
    | If(e, s1, [], s2) ->
      "if " ^ string_of_expr e ^ "\n" ^
      "{\n" ^
      string_of_stmt_list s1 ^ "\n" ^
      "}\n" ^
      "else\n" ^
      string_of_stmt_list s2
    | If (e, s, elifs, []) -> 
      "if " ^ string_of_expr e ^ "\n" ^
      "{\n" ^
      string_of_stmt_list s ^
      "}\n" ^
      String.concat "\n" (List.map (fun elif -> "elif " ^ string_of_expr (fst elif) ^ "\n" ^
                                              "{\n" ^
                                              string_of_stmt_list (snd elif) ^
                                              "}") elifs)
    | If (e, s1, elifs, s2) -> 
      "if " ^ string_of_expr e ^ "\n" ^
      "{\n" ^
      string_of_stmt_list s1 ^
      "}\n" ^
      (String.concat "\n" (List.map (fun elif -> "elif " ^ string_of_expr (fst elif) ^ "\n" ^
                                              "{\n" ^
                                              string_of_stmt_list (snd elif) ^
                                              "}") elifs)) ^ "\n" ^
                                              "else\n" ^
                                              "{\n" ^
                                              string_of_stmt_list s2 ^
                                              "}"
    | While(e, s) ->
        "while " ^ string_of_expr e ^ "\n" ^
        "{\n" ^
        string_of_stmt_list s ^
        "}"
    | For(id, e1, e2, s) ->
        "for " ^ id ^ " from " ^ string_of_expr e1 ^ " to " ^ string_of_expr e2 ^ "\n" ^
        "{\n" ^
        string_of_stmt_list s ^
        "}"
    | Break -> "break;"
    | Continue -> "continue;"
    | Bind(o, p, f) -> "bind( " ^ o ^ "." ^ p ^ ", " ^ f ^ ");"
    | Unbind(o, p, f) -> "unbind( " ^ o ^ "." ^ p ^", " ^ f ^ ");") ^
    "\n" ^ string_of_stmt_list sts

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";"
let string_of_vdecls = function
    [] -> ""
  | vdecls -> String.concat "\n" (List.rev (List.map string_of_vdecl vdecls)) ^ "\n"

let string_of_odecl odecl =
  "objdef " ^ odecl.oname ^ "\n" ^
  "{\n" ^
  String.concat "\n" (List.map string_of_vdecl odecl.props) ^ "\n" ^
  "}"

let string_of_formal (t, id) = string_of_typ t ^ " " ^ id

let string_of_fdecl fdecl =
  "fn " ^ fdecl.fname ^ "("  ^ String.concat ", " (List.map string_of_formal fdecl.formals) ^ ") -> " ^ string_of_typ fdecl.typ ^ "\n" ^
  "{\n" ^
  string_of_vdecls fdecl.locals ^
  string_of_stmt_list fdecl.body ^
  "}"

let string_of_program (vdecls, odecls, fdecls) =
  string_of_vdecls vdecls ^
  String.concat "\n" (List.rev (List.map string_of_odecl odecls)) ^ "\n" ^
  String.concat "\n" (List.rev (List.map string_of_fdecl fdecls))
