{
        open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = digit+
let id = alpha (digit | alpha | '_')*

let ascii_char = ['\x00' - '\x7F']
let string = '"' ((ascii_char)* as s) '"'

let newline = '\n' 

rule token = parse
 [' ' '\t' '\r' '\n'] { token lexbuf }
(*| newline { incr depth; token lexbuf }*)
| "//" { comment lexbuf}
| "vector" { VECTOR }
| "diagonalLeft" { DIAGLEFT }
| "diagonalRight" { DIAGRIGHT }
| "horizontal" { HORIZONTAL }
| "vertical" { VERTICAL }
| "matrix" { MATRIX }
| "matrix_create" { MATRIX_C }
| "move" { MOVE }


| "Duple" {DUPLE}

| "bool" { BOOL }
| "int" { INT }
| "true" { BLIT(true) }
| "false" { BLIT(false) }

| "string" { STRING }

| "tuple" { TUPLE }

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
| string as lem{ STRUCT(lem)}

| eof { EOF }
|  _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
newline { token lexbuf }
 | _ { comment lexbuf }

