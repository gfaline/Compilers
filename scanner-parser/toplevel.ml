open Ast
let rec run = function
    [] -> ()
  | t::ts -> print_endline t; run ts

let () =
  let usage_msg = "usage: ./toplevel.native [file.pr]" in
  let channel = ref stdin in
  Arg.parse [] (fun file -> channel:= open_in file) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  print_string (Ast.string_of_program ast)
  
(* let () =
  let lex_buf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lex_buf in
  run prg *)