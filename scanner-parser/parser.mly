/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token SEMI
%token INT BOOL FLOAT STR VOID
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

vtyp:
    INT   { Int }
  | BOOL  { Bool }
  | FLOAT { Float}
  | STR   { Str }

ftyp:
    INT   { Int }
  | BOOL  { Bool }
  | FLOAT { Float}
  | STR   { Str }
  | VOID  { Void }


vdecl:
    vtyp ID SEMI { ($1, $2) }