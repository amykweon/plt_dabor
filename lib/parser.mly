%{ open Ast %}
%token DOT SEMI COLON COMMA LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS MULTIPLY DIVIDE MOD ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE WHILE CONTINUE BREAK BOOL INT STRING DUPLE 
%token VECTOR DIAGLEFT DIAGRIGHT HORIZONTAL VERTICAL MATRIX MATRIX_C MOVE TUPLE
%token <int> INT_LITERAL 
%token <bool> BLIT 
%token <string> ID 
%token <string> STRING_LITERAL
%token <string> STRUCT

%token EOF

%right ASSIGN
%left NOT
%left OR
%left AND
%left EQ NEQ LT LEQ GT GEQ
%left MULTIPLY DIVIDE MOD
%left PLUS MINUS
%left MOVE

%start program_rule
%type <Ast.program> program_rule

%nonassoc NOELSE
%nonassoc ELSE

%%

program_rule:
   vdecl_list_rule stmt_list_rule EOF { {locals =  $1; body =  $2} }

vdecl_list_rule:
   /*nothing*/                   { [] }
 | vdecl_rule vdecl_list_rule    { $1 :: $2 }

vdecl_rule:
   typ_rule ID SEMI                          { ($1, $2) }
 /* | StructT ID LBRACE struct_def_list RBRACE   { StructDef($2, $4) }*/
typ_rule:
   INT         { Int  }
 | BOOL        { Bool }
 | STRING      { String }
 | MATRIX      { Matrix }
 | VECTOR      { Vector }
 | DUPLE       { Duple }
 /* | STRUCT    { Struct }*/

struct_def_list:
   ID COLON typ_rule                         { [$1, $3] }
 | ID COLON typ_rule COMMA struct_def_list   { ($1, $3) :: $5 }

struct_list:
   ID COLON expr_rule                     { [$1, $3] }
 | ID COLON expr_rule COMMA struct_list   { ($1, $3) :: $5 }
     
stmt_list_rule:
   /* nothing */               { [] }
 | stmt_rule stmt_list_rule    { $1::$2 }

stmt_rule:
   expr_rule SEMI                                        { Expr $1 }
 | LBRACE stmt_list_rule RBRACE                          { Block $2 }
 | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
 | IF LPAREN expr_rule RPAREN stmt_rule %prec NOELSE     {If ($3, $5, Block([]))}
 | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5) }

rest_of_list_rule:
   INT_LITERAL COMMA rest_of_list_rule                 { $1::$3 }
//  | STRING_LITERAL COMMA rest_of_list_rule              { $1::$3 }
 | INT_LITERAL RBRACK /* no empty lists allowed */     { $1::[] }
//  | STRING_LITERAL RBRACK /* no empty lists allowed */  { $1::[] }

list_rule:
   LBRACK rest_of_list_rule { $2 }

rest_of_matrix_rule:
   list_rule COMMA rest_of_matrix_rule  { $1::$3 }
 | list_rule RBRACK                     { $1::[] }

matrix_rule:
   LBRACK rest_of_matrix_rule  { $2 }
/*cann't do like this */
id_rule:
   ID                                    { Id $1 }
 | ID DOT ID                             { StructAccess($1, $3) }
 | ID LBRACK INT_LITERAL COMMA INT_LITERAL RBRACK { MatrixAccess($1, $3, $5) }

expr_rule:
   BLIT                                  { BoolLit $1 }
 | INT_LITERAL                           { IntLit $1 }
 | STRING_LITERAL                        { StringLit $1 }
 | STRUCT                                 {Struct $1}
 | ID                                    { Id $1 }
 | ID ASSIGN expr_rule              { Assign ($1, $3) }
 | expr_rule PLUS expr_rule              { Binop ($1, Add, $3) }
 | expr_rule MINUS expr_rule             { Binop ($1, Sub, $3) }
 | expr_rule MULTIPLY expr_rule          { Binop ($1, Multi, $3) }
 | expr_rule DIVIDE expr_rule            { Binop ($1, Divide, $3) }
 | expr_rule EQ expr_rule                { Binop ($1, Equal, $3) }
 | expr_rule NEQ expr_rule               { Binop ($1, Neq, $3) }
 | expr_rule LT expr_rule                { Binop ($1, Less, $3) }
 | expr_rule LEQ expr_rule               { Binop ($1, EqLess, $3) } 
 | expr_rule GT expr_rule                { Binop ($1, Greater, $3) }
 | expr_rule GEQ expr_rule               { Binop ($1, EqGreater, $3) }
 | expr_rule AND expr_rule               { Binop ($1, And, $3) }
 | expr_rule OR expr_rule                { Binop ($1, Or, $3) }
 | expr_rule MOD expr_rule               { Binop ($1, Mod, $3) }
 | expr_rule MOVE expr_rule              { Binop ($1, Move, $3) }
 | NOT expr_rule                         { Unop(Not, $2) }
 | DIAGLEFT LPAREN INT_LITERAL RPAREN    { VectorCreate(DiagL, $3) }
 | DIAGRIGHT LPAREN INT_LITERAL RPAREN   { VectorCreate(DiagR, $3) }
 | HORIZONTAL LPAREN INT_LITERAL RPAREN  { VectorCreate(Hori, $3) }
 | VERTICAL LPAREN INT_LITERAL RPAREN    { VectorCreate(Vert, $3) }
 | LPAREN expr_rule RPAREN               { $2 }
 | LBRACE struct_list RBRACE             { StructCreate($2) }
 | ID DOT ID                             { StructAccess($1, $3) }
 | MATRIX_C LPAREN matrix_rule RPAREN    { MatrixCreate($3) }
 | ID LBRACK INT_LITERAL COMMA INT_LITERAL RBRACK { MatrixAccess($1, $3, $5) }
 /*| ID LBRACK id_rule RBRACK                       { MatrixAccess($1, $3) }
   misssing arguments
 */
 | LPAREN INT_LITERAL COMMA INT_LITERAL RPAREN    { DupleCreate($2, $4) }
