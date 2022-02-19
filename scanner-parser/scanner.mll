(* scanner for Propeller language *)

{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit  = ['0'-'9']
let digits = digit+
(* valid and invalid names *)
let valid_name   = letter (letter | digit | '_')* (letter | digit | '?')
let valid_sym    = (letter | digit | '_' | '?')
let invalid_name =
    '?' valid_sym*
  | '_' valid_sym*
  | valid_sym* '_'
  | valid_sym* "_?"
  | valid_sym* "__" valid_sym*
  | valid_sym* "?" valid_sym+

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | "//" { comment lexbuf }
  | invalid_name as tkn { raise (Failure("illegal name " ^ tkn)) }
  | valid_name   as tkn { TKN(tkn) }
  | eof { EOF }
  | '~' { EOF }
  (* | bad_name as bn { raise (Failure("Bad question mark in " ^ bn)) } *)
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

  and comment = parse
      '\n' { token lexbuf }
    | _    { comment lexbuf }