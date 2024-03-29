{
        open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = digit+
let id = alpha (digit | alpha | '_')*

let string = '"' ((digit | alpha | '_' | ' ')* as s) '"'

let newline = '\n' 

rule token = parse
 [' ' '\t' '\r' '\n'] { token lexbuf }
| "//" { comment lexbuf}
| "vector" { VECTOR }
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

| "print_int" { PRINTI }
| "print_string" { PRINTS }
| "print_matrix" { PRINTM }
| "print_duple" { PRINTD }
| "print_vector" { PRINTV }

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
| '"' { raise (Failure("unmatched quotation")) }
| eof { EOF }
|  _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
newline { token lexbuf }
 | _ { comment lexbuf }