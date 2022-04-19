open Ast

type sobj_decl = {
  soname : string;
  sprops : bind list;
  sextern : bool }

type sexpr = typ * sx
and sx =
    SIliteral of int
  | SFliteral of float
  | SBliteral of bool
  | SSliteral of string
  | SLliteral of sexpr array
  | SCall of string * sexpr list
  | SAssign of string * sexpr
  | SSetprop of string * string * sexpr
  | SId of string
  | SGetprop of string * string
  | SIndex of string * sexpr
  | SBinop of sexpr * binop * sexpr
  | SUnop of unop * sexpr
  | SParentheses of sexpr
  | SNoexpr

type sstmt =
    SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt list * (sexpr * sstmt list) list * sstmt list
  | SFor of string * sexpr * sexpr * sstmt list
  | SWhile of sexpr * sstmt list
  | SBreak
  | SContinue
  | SBind of string * string * string
  | SUnbind of string * string * string

type sfunc_decl = {
  styp : typ;
  sfname : string;
  sformals : bind list;
  slocals : bind list;
  sbody : sstmt list }

type sprogram = bind list * obj_decl list * sfunc_decl list

let rec string_of_sexpr (t, e) = "(" ^ string_of_typ t ^ " : " ^ (match e with 
    SIliteral x -> string_of_int x
  | SFliteral x -> string_of_float x
  | SBliteral x -> if x then "true" else "false"
  | SSliteral x -> x
  | SLliteral xs -> "[" ^ String.concat ", " (Array.to_list (Array.map string_of_sexpr xs)) ^ "]"
  | SCall (f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"
  | SAssign (id, e) -> id ^ " = " ^ string_of_sexpr e
  | SSetprop (o, p, e) -> o ^ "." ^ p ^ " = " ^ string_of_sexpr e
  | SId id -> id
  | SGetprop (o, p) -> o ^ "." ^ p
  | SIndex (id, e) -> id ^ "[" ^ string_of_sexpr e ^ "]"
  | SBinop (e1, op, e2) -> string_of_sexpr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_sexpr e2
  | SUnop (op, e) -> (match op with
      Not -> string_of_unop op ^ " (" ^ string_of_sexpr e ^ ")"
    | Neg -> string_of_unop op ^ "(" ^ string_of_sexpr e ^ ")")
  | SParentheses e -> "(" ^ string_of_sexpr e ^ ")"
  | SNoexpr -> "") ^ ")"

  let string_of_sodecl odecl =
    if odecl.sextern then
      "external objdef " ^ odecl.soname ^ "\n" ^
      brace_wrap (String.concat "\n" (List.map string_of_vdecl odecl.sprops))
    else
      "objdef " ^ odecl.soname ^ "\n" ^
      brace_wrap (String.concat "\n" (List.map string_of_vdecl odecl.sprops))

let rec string_of_sstmt = function
    SExpr e -> string_of_sexpr e ^ ";"
  | SReturn e -> (match e with
        (Void, SNoexpr) -> "return;"
      | _            -> "return " ^ string_of_sexpr e ^ ";")
  | SIf (e, s1, elifs, s2) ->
      let if_str =
        "if " ^ string_of_sexpr e ^ "\n" ^
        brace_wrap (String.concat "\n" (List.map string_of_sstmt s1)) in
      let string_of_elif (elif_e, elif_s) = 
        "elif " ^ string_of_sexpr elif_e ^ "\n" ^
        brace_wrap(String.concat "\n" (List.map string_of_sstmt elif_s))
      in
      let elif_str = match elifs with 
          [] -> ""
        | _  -> "\n" ^
                String.concat "\n" (List.map string_of_elif elifs) in
      let else_str = match s2 with
          [] -> ""
        | _  -> "\n" ^
                "else\n" ^
                brace_wrap(String.concat "\n" (List.map string_of_sstmt s2)) in
      if_str ^ elif_str ^ else_str
  | SFor (id, e1, e2, s) ->
      "for " ^ id ^ " from " ^ string_of_sexpr e1 ^ " to " ^ string_of_sexpr e2 ^ "\n" ^
      brace_wrap (String.concat "\n" (List.map string_of_sstmt s))
  | SWhile (e, s) ->
        "while " ^ string_of_sexpr e ^ "\n" ^
        brace_wrap (String.concat "\n" (List.map string_of_sstmt s))
  | SBreak -> "break;"
  | SContinue -> "continue;"
  | SBind (o, p, f) -> "bind(" ^ o ^ "." ^ p ^ ", " ^ f ^ ");"
  | SUnbind(o, p, f) -> "unbind( " ^ o ^ "." ^ p ^", " ^ f ^ ");"

  (* | _ -> "NONE" *)

let string_of_sfdecl fdecl =
  "fn " ^ fdecl.sfname ^ "("  ^ String.concat ", " (List.map snd fdecl.sformals) ^ ") -> " ^ string_of_typ fdecl.styp ^ "\n" ^
  brace_wrap ((String.concat "\n" (List.rev (List.map string_of_vdecl fdecl.slocals))) ^ "\n\n" ^
               String.concat "\n" (List.map string_of_sstmt fdecl.sbody))

let string_of_sprogram (vdecls, odecls, fdecls) =
  String.concat "\n" (List.rev (List.map string_of_vdecl vdecls)) ^ "\n\n" ^
  String.concat "\n\n" (List.rev (List.map string_of_sodecl odecls)) ^ "\n\n" ^
  String.concat "\n\n" (List.rev (List.map string_of_sfdecl fdecls))