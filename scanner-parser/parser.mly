/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token EOF
%token <string> TKN


%start program
%type <Ast.program> program

%%

program:
    TKN { Prog($1) }