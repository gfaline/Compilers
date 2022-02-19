open Ast

let run = function
    Prog(p) -> ("Begin\n" ^ p ^ "\nEnd\n")

let () =
  let lex_buf = Lexing.from_channel stdin in
  let prg = Parser.program Scanner.token lex_buf in
  let result = run prg in
  print_endline result