/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token SEMI
%token INT BOOL FLOAT STR
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
    INT   { Int }
  | BOOL  { Bool }
  | FLOAT { Float}
  | STR   { Str }

vdecl:
    typ ID SEMI { ($1, $2) }