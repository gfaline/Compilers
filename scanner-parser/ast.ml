type typ = Int | Bool | Float | Str

type bind = typ * string

type program = bind list

let string_of_typ = function
    Int   -> "int"
  | Bool  -> "bool"
  | Float -> "float"
  | Str   -> "str"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_program vdecls =
  String.concat "" (List.map string_of_vdecl vdecls) ^ "\n"
