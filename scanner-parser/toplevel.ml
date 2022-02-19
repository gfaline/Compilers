open Ast

(* let run tkn_list = function
    Prog(p) -> p :: tkn_list *)

(* let rec print_tkn_list = function
    [] -> ()
  | t::ts -> print_endline(t); print_tkn_list ts *)

  let rec run = function
      [] -> ()
    | t::ts -> print_endline t; run ts

let () =
  let lex_buf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lex_buf in
  run prg
  (* let result = run [] prg in *)
  (* print_tkn_list result *)