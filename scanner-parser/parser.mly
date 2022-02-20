/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token SEMI
%token INT
%token <string> ID
%token EOF



%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls:
  /* nothing */ { [] }
  | decls vdecl { $2 :: $1}

typ:
    INT { Int }

vdecl:
    typ ID SEMI { ($1, $2) }