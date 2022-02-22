/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA OBJDEF FN ARROW ASSIGN PLUS MINUS TIMES DIVIDE MODULO
%token NOT EQ NEQ LT LEQ GT GEQ XOR AND OR
%token BIND UNBIND BREAK CONTINUE RETURN IF ELIF ELSE FOR FROM TO WHILE OBJ INT BOOL FLOAT STR VOID LIST
%token PERIOD
%token <int> ILIT
%token <float> FLIT
%token <bool> BLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc ELIF
%right ASSIGN
%left OR
%left AND
%left XOR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT

%%

program:
    decls EOF { $1 }

decls:
    /* nothing */ { ([], [], []) }
  | decls vdecl   { (($2 :: fst_trpl $1), snd_trpl $1, trd_trpl $1) }
  | decls odecl   { (fst_trpl $1, ($2 :: snd_trpl $1), trd_trpl $1) }
  | decls fdecl   { (fst_trpl $1, snd_trpl $1, ($2 :: trd_trpl $1)) }

odecl:
  OBJDEF ID LBRACE vdecl_list RBRACE
    { { oname = $2;
        props = List.rev $4 } }

fdecl:
  FN ID LPAREN formals_opt RPAREN ARROW typ LBRACE vdecl_list stmt_list RBRACE
    { { typ     = $7;
        fname   = $2;
        formals = List.rev $4;
        locals  = List.rev $9;
        body    = List.rev $10 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1, $2)] }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
    INT   { Int }
  | BOOL  { Bool }
  | FLOAT { Float}
  | STR   { Str }
  | VOID  { Void }
  | OBJ   { Obj }
  | typ LIST { List($1) }

vdecl_list:
    /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }
  
vdecl:
    typ ID SEMI { ($1, $2) }

stmt_list:
    // /* nothing */ { [] }
    stmt { [$1] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr_stmt   { $1 }
  | return_stmt { $1 }
  | if_stmt     { $1 }
  | for_stmt    { $1 }
  | while_stmt  { $1 }
  | BREAK SEMI    { Break }
  | CONTINUE SEMI { Continue }
  | bind_stmt   { $1 }
  | unbind_stmt { $1 }

bind_stmt:
  BIND LPAREN ID COMMA ID RPAREN SEMI { Bind ($3, $5) }
unbind_stmt:
  UNBIND LPAREN ID COMMA ID RPAREN SEMI { Bind ($3, $5) }

expr_stmt:
    expr SEMI { Expr($1) }

return_stmt:
  | RETURN expr_opt SEMI { Return($2) }
  
if_stmt:
    IF expr LBRACE stmt_list RBRACE elif_stmts else_stmt { If($2, List.rev $4, $6, $7) }

else_stmt:
    %prec NOELSE { [] }
  | ELSE LBRACE stmt_list RBRACE    { List.rev $3 }

elif_stmts:
    /* nothing */ { [] }
  | elif_stmts elif_stmt { $2 :: $1 }

elif_stmt:
  ELIF expr LBRACE stmt_list RBRACE { ($2, List.rev $4) }

for_stmt:
    FOR ID FROM expr TO expr LBRACE stmt_list RBRACE { For($2, $4, $6, List.rev $8) }

while_stmt:
    WHILE expr LBRACE stmt_list RBRACE  { While($2, List.rev $4) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1     }

expr:
    ILIT { Iliteral($1) }
  | FLIT { Fliteral($1) }
  | BLIT { Bliteral($1) }
  | SLIT { Sliteral($1) }
  | ID   { Id($1) }
  | expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mlt, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MODULO expr { Binop($1, Mod, $3) }
  | expr EQ     expr { Binop($1, Eq,  $3) }
  | expr NEQ    expr { Binop($1, Neq, $3) }
  | expr LT     expr { Binop($1, Lt,  $3) }
  | expr LEQ    expr { Binop($1, Leq, $3) }
  | expr GT     expr { Binop($1, Gt,  $3) }
  | expr GEQ    expr { Binop($1, Geq, $3) }
  | expr AND    expr { Binop($1, And, $3) }
  | expr XOR    expr { Binop($1, Xor, $3) }
  | expr OR     expr { Binop($1, Or,  $3) }
  | MINUS expr %prec NOT { Unop(Neg, $2)  }
  | NOT  expr        { Unop(Not, $2)      }
  | ID ASSIGN expr { Assign($1, $3) }
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID PERIOD ID %prec NOT { Getprop($1, $3) }
  | ID PERIOD ID ASSIGN expr { Setprop($1, $3, $5) }

args_opt:
    /* nothing */ { [] }
  | args_list     { List.rev $1 }

args_list:
    expr                 { [$1]     }
  | args_list COMMA expr { $3 :: $1 }