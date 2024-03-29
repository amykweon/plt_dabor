%{ open Ast %}
%token DOT SEMI COLON COMMA LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS MULTIPLY MOD ASSIGN PRINTI PRINTS PRINTM PRINTD PRINTV
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE WHILE CONTINUE BREAK BOOL INT STRING DUPLE STRUCT
%token VECTOR MATRIX MATRIX_C MOVE TUPLE
%token <int> INT_LITERAL 
%token <bool> BLIT 
%token <string> ID 
%token <string> STRING_LITERAL

%token EOF

%right ASSIGN
%left NOT
%left OR
%left AND
%left EQ NEQ LT LEQ GT GEQ
%left MULTIPLY MOD
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
   typ_rule ID SEMI                          { Bind($1, $2) }
 | STRUCT ID LBRACE struct_def_list RBRACE SEMI   { StructDef($2, $4) }

typ_rule:
   INT         { Int  }
 | BOOL        { Bool }
 | STRING      { String }
 | MATRIX LBRACK INT_LITERAL RBRACK LBRACK INT_LITERAL RBRACK   { Matrix($3, $6) }
 | VECTOR      { Vector }
 | DUPLE       { Duple }
 | STRUCT ID   { StructT($2) }

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
 | INT_LITERAL RBRACK /* no empty lists allowed */     { $1::[] }

list_rule:
   LBRACK rest_of_list_rule { $2 }

rest_of_matrix_rule:
   list_rule COMMA rest_of_matrix_rule  { $1::$3 }
 | list_rule RBRACK                     { $1::[] }

matrix_rule:
   LBRACK rest_of_matrix_rule  { $2 }

id_rule:
  ID                                    { Id $1 }
| ID DOT ID                             { StructAccess($1, $3) }
| ID LBRACK INT_LITERAL RBRACK          { DupleAccess($1, $3) }
| ID LBRACK INT_LITERAL COMMA INT_LITERAL RBRACK { IndexAccess($1, $3, $5) }
| ID LBRACK id_rule RBRACK              { IndexAccessVar($1, $3) }

expr_rule:
   BLIT                                  { BoolLit $1 }
 | INT_LITERAL                           { IntLit $1 }
 | STRING_LITERAL                        { StringLit $1 }
 | id_rule                               { IdRule $1 }
 | id_rule ASSIGN expr_rule              { Assign ($1, $3) }
 | expr_rule PLUS expr_rule              { Binop ($1, Add, $3) }
 | expr_rule MINUS expr_rule             { Binop ($1, Sub, $3) }
 | expr_rule MULTIPLY expr_rule          { Binop ($1, Multi, $3) }
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
 | MINUS expr_rule                       { Unop (Neg, $2) }
 | NOT expr_rule                         { Unop (Not, $2) }
 | LT expr_rule COMMA expr_rule GT       { VectorCreate($2, $4) }
 | LPAREN expr_rule RPAREN               { $2 }
 | ID LBRACE struct_list RBRACE          { StructCreate($1, $3) }
 | MATRIX_C LPAREN matrix_rule RPAREN    { MatrixCreate($3) }
 | LPAREN expr_rule COMMA expr_rule RPAREN    { DupleCreate($2, $4) }
 | PRINTI LPAREN expr_rule RPAREN      { PrintInt($3) }
 | PRINTS LPAREN expr_rule RPAREN      { PrintStr($3) }
 | PRINTM LPAREN id_rule RPAREN        { PrintMat($3) }
 | PRINTD LPAREN expr_rule RPAREN      { PrintDup($3) }
 | PRINTV LPAREN expr_rule RPAREN      { PrintVec($3) }