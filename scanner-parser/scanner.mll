(* scanner for Propeller language *)

{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit  = ['0'-'9']
let digits = digit+

(* valid and invalid names *)
let valid_name   = letter (letter | digit | '_')* (letter | digit | '?')
let valid_sym    = (letter | digit | '_' | '?')
let invalid_name =
    '?' valid_sym*  (* starts with ? *)
  | '_' valid_sym*  (* starts with _ *)
  | valid_sym* '_'  (* ends with _ *)
  | valid_sym* "_?" (* ends with -? *)
  | valid_sym* "__" valid_sym* (* contains one or more consecutive underscores *)
  | valid_sym* '?' valid_sym+  (* contains a question mark before the final character *)

(* parse input *)
rule token = parse
  (* whitespace/comments *)
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | "//" { comment lexbuf }
  (* syntactical characters *)
  | ';' { SEMI }
  (* primitive types *)
  | "int"    { INT }
  | "bool"   { BOOL }
  | "float"  { FLOAT }
  | "str"    { STR }
  (* | digits       as d  { LITERAL(int_of_string d) } *)
  | invalid_name as id { raise (Failure("illegal name " ^ id)) }
  | valid_name   as id { ID(id) }
  | eof { EOF }
  | '~' { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

  and comment = parse
      '\n' { token lexbuf }
    | _    { comment lexbuf }