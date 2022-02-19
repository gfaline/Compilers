/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token EOF
%token <string> TKN


%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
  /* nothing */ { [] }
  | decls TKN   {$2 :: $1 }