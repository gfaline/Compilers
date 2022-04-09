open Ast

type sexpr = typ * sx
and sx =
    SIliteral of int
  | SFliteral of float
  | SBliteral of bool
  | SSliteral of string
  (* | SLliteral of sexpr list *)
  | SCall of string * sexpr list
  (* | SAssign of string * sexpr *)
  | SBinop of sexpr * binop * sexpr
  | SUnop of unop * sexpr

type sstmt =
    SExpr of sexpr
  | SReturn of sexpr

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
  | SBliteral x -> string_of_bool x
  | SSliteral x -> x
  | SCall (f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"
  (* | SAssign (id, e) -> id ^ " = " ^ string_of_sexpr e ^ ";" *)
  | SBinop (e1, op, e2) -> string_of_sexpr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_sexpr e2
  | SUnop (op, e) -> (match op with
      Not -> string_of_unop op ^ " (" ^ string_of_sexpr e ^ ")"
    | Neg -> string_of_unop op ^ "(" ^ string_of_sexpr e ^ ")")
  | _ -> "NONE") ^ ")"

let string_of_sodecl _ = "NONE"

let rec string_of_sstmt_list = function
    [] -> ""
  | st::sts -> (match st with
        SExpr e -> string_of_sexpr e ^ ";"
      | _ -> "NONE") ^ "\n" ^ string_of_sstmt_list sts

let string_of_sfdecl fdecl =
  "fn " ^ fdecl.sfname ^ "("  ^ String.concat ", " (List.map snd fdecl.sformals) ^ ") -> " ^ string_of_typ fdecl.styp ^ "\n" ^
  "{\n" ^
  string_of_vdecls fdecl.slocals ^
  string_of_sstmt_list fdecl.sbody ^
  "}"

let string_of_sprogram (vdecls, odecls, fdecls) =
  string_of_vdecls vdecls ^
  String.concat "\n" (List.rev (List.map string_of_sodecl odecls)) ^ "\n" ^
  String.concat "\n" (List.rev (List.map string_of_sfdecl fdecls)) ^ "\n"
