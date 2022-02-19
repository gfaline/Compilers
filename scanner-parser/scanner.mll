(* scanner for Propeller language *)

{ open Parser }

rule token = parse
    (* [' ' '\t' '\r' '\n'] { token lexbuf } *)
  | eof { EOF }
  | '~' { EOF }
  | ['a' - 'z']* as tkn { TKN(tkn) }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }