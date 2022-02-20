type typ = Int

type bind = typ * string

type program = bind list

let string_of_typ = function
    Int -> "int"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_program vdecls =
  String.concat "" (List.map string_of_vdecl vdecls) ^ "\n"
