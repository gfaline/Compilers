(* scanner for Propeller language *)

{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit  = ['0'-'9']
let alphnum = letter | digit
let identifier = letter '?'?
         | letter '_'? ( alphnum | alphnum '_')* alphnum '?'?

(* parse input *)
rule token = parse
  (* whitespace/comments *)
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | '#' { comment lexbuf }
  (* syntactical symbols *)
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '['      { LBRCKT }
  | ']'      { RBRCKT }
  | ';'      { SEMI }
  | ','      { COMMA }
  | "fn"     { FN }
  | "->"     { ARROW }
  | '.'      { PERIOD }
  (* arithemtic operators *)
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | '%'      { MODULO }
  | '='      { ASSIGN }
  (* comparison operators *)
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  (* logical operators *)
  | "not"    { NOT }
  | "xor"    { XOR }
  | "and"    { AND }
  | "or"     { OR  }
  (* control flow *)
  | "for"    { FOR }
  | "from"   { FROM }
  | "to"     { TO }
  | "if"     { IF }
  | "elif"   { ELIF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "break"  { BREAK }
  | "continue" { CONTINUE }
  | "return" { RETURN }
  (* Propeller stuff *)
  | "objdef" { OBJDEF }
  | "bind"   { BIND }
  | "unbind" { UNBIND }
  | "external" { EXTERNAL }
  (* primitive types *)
  | "obj"    { OBJ }
  | "int"    { INT }
  | "bool"   { BOOL }
  | "float"  { FLOAT }
  | "str"    { STR }
  | "void"   { VOID }
  | "list"   { LIST }
  (* literals *)
  | digit+ as x            { ILIT(int_of_string x)   } 
  | digit+ '.' digit+ as x { FLIT(float_of_string x) }
  | "true"                 { BLIT(true) }
  | "false"                { BLIT(false) }
  | ''' [^''']*'''    as s { SLIT(s) }
  (* names *)
  | identifier   as id { ID(id) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

  and comment = parse
      '\n' { token lexbuf }
    | eof  { token lexbuf }
    | _    { comment lexbuf }