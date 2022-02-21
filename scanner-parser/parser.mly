/* Ocamlyacc parser for Propeller */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA FN ARROW ASSIGN PLUS MINUS TIMES DIVIDE MODULO
%token NOT EQ NEQ LT LEQ GT GEQ XOR AND OR
%token RETURN IF ELIF ELSE FOR FROM TO WHILE INT BOOL FLOAT STR VOID
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
    /* nothing */ { ([], [])               }
  | decls vdecl   { (($2 :: fst $1), snd $1) }
  | decls fdecl   { (fst $1, ($2 :: snd $1)) }

fdecl:
  FN ID LPAREN formals_opt RPAREN ARROW ftyp LBRACE vdecl_list stmt_list RBRACE
    { { typ     = $7;
        fname   = $2;
        formals = List.rev $4;
        locals  = List.rev $9;
        body    = List.rev $10 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    vtyp ID                   { [($1, $2)] }
  | formal_list COMMA vtyp ID { ($3, $4) :: $1 }

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

vdecl_list:
    /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }
  
vdecl:
    vtyp ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI            { Expr($1) }
  | RETURN expr_opt SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block (List.rev $2) }
  | if_stmt    { $1 }
  | for_stmt   { $1 }
  | while_stmt { $1 }
  
if_stmt:
    IF LPAREN expr RPAREN stmt elif_stmts else_stmt { If($3, $5, $6, $7) }

else_stmt:
    %prec NOELSE { Block([]) }
  | ELSE stmt    { $2 }

elif_stmts:
    /* nothing */ { [] }
  | elif_stmts elif_stmt { $2 :: $1 }

elif_stmt:
  ELIF LPAREN expr RPAREN stmt { ($3, $5)}

for_stmt:
    FOR LPAREN ID FROM expr TO expr RPAREN stmt { For($3, $5, $7, $9) }

while_stmt:
    WHILE LPAREN expr RPAREN stmt  { While($3, $5) }

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

args_opt:
    /* nothing */ { [] }
  | args_list     { List.rev $1 }

args_list:
    expr                 { [$1]     }
  | args_list COMMA expr { $3 :: $1 }