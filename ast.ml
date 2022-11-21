(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mod | Multi | Divide | Equal | Neq | Less | EqLess | Greater | EqGreater | And | Or | Move | Not

type dir = DiagL | DiagR | Hori | Vert

type typ = Int | Bool | String | Vector | Matrix | StructT of string | Duple

type expr =
    IntLit of int
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | VectorCreate of dir * expr
  | Binop of expr * op * expr
  | Unop of op * expr
  | Assign of string * expr
  | MatrixCreate of int list list
  | MatrixAccess of string * int * int
  | StructCreate of (string * expr) list
  | StructAccess of string * string
  | DupleCreate of int * int

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

// TODO: find a way to combine StructDef and bind into decl
type bind = typ * string

type decl =
    bind
  | StructDef of string * bind list

type program = {
   locals: decl list;
   body: stmt list;
}

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mod -> "%"
  | Multi -> "*"
  | Divide -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | EqLess -> "<="
  | Greater -> ">"
  | EqGreater -> ">="
  | And -> "&&"
  | Or -> "||" 

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "\n" (List.map string_of_stmt fdecl.body) ^
  "\n"

