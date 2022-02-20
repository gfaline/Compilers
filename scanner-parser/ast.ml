type typ = Int | Bool | Float | Str | Void

type bind = typ * string

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list; 
    locals : bind list }

type program = bind list * func_decl list

let string_of_typ = function
    Int   -> "int"
  | Bool  -> "bool"
  | Float -> "float"
  | Str   -> "str"
  | Void  -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "fn " ^ fdecl.fname ^ "("  ^ String.concat ", " (List.map snd fdecl.formals) ^ ") -> " ^ string_of_typ fdecl.typ ^ "\n" ^
  "{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  "}\n"

let string_of_program (vdecls, fdecls) =
  String.concat "" (List.map string_of_vdecl vdecls) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl fdecls)
