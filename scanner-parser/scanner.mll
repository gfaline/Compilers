(* scanner for Propeller language *)

{ open Parser }

rule token = parse
    (* [' ' '\t' '\r' '\n'] { token lexbuf } *)
    "//" { comment lexbuf }
  | ['a' - 'z']* as tkn { TKN(tkn) }
  | eof { EOF }
  | '~' { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

  and comment = parse
      '\n' { token lexbuf }
    | _    { comment lexbuf }