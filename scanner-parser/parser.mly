/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token EOF
%token <string> ID


%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
  /* nothing */ { [] }
  | decls ID   {$2 :: $1 }