{
        open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = digit+
let id = alpha (digit | alpha | '_')*

let ascii_char = ['\x00' - '\x7F']
let string = '"' ((ascii_char)* as s) '"'

rule token = parse
 [' ' '\t' '\r'] { token lexbuf }
| newline { incr depth; token lexbuf }
| "//" { comment lexbuf}
| "vector" { VECTOR }
| "diagonalLeft" { DIAGLEFT }
| "diagonalRight" { DIAGRIGHT }
| "horizontal" { HORIZONTAL }
| "vertical" { VERTICAL }
| "matrix" { MATRIX }
| "matrix_create" { MATRIX_C }
| "move" { MOVE }

| "bool" { BOOL }
| "int" { INT }
| "true" { BLIT(true) }
| "false" { BLIT(false) }

| "string" { STRING }
| "struct" { STRUCT }
| "tuple" { TUPLE }
| "duple" {DUPLE}

| "if"  { IF }
| "else" { ELSE}
| "while" { WHILE }
| "continue" { CONTINUE }
| "break" { BREAK }

| "and" { AND }
| "or" { OR }
| "not" { NOT }

| '.' { DOT }

| '(' { LPAREN } 
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACK }
| ']' { RBRACK }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }

| '+' { PLUS }
| '-' { MINUS }
| '*' { MULTIPLY }
| '/' { DIVIDE }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| '>' { GT }
| ">=" { GEQ }
| '%'  { MOD }

| digit+ as lem { INT_LITERAL(int_of_string lem) }
| string { STRING_LITERAL(s) }
| id as lem { ID(lem) }
| '"' { raise (Exceptions.UnmatchedQuotation(!lineno)) }
| _ as illegal { raise (Exceptions.IllegalCharacter(!filename, illegal, !lineno)) }

| eof { EOF }
|  _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
newline { token lexbuf }
 | _ { comment lexbuf }