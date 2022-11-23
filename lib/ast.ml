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

type s_ele = string * typ

type decl =
    Bind of typ * string
  | StructDef of string * s_ele list

type program = {
   locals: decl list;
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
  | Not -> "!"
  | Move -> "move" 

let string_of_dir = function
    DiagL -> "diagl"
  | DiagR -> "diagr"
  | Hori -> "hori"
  | Vert -> "vert"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_expr e ^ " " ^ string_of_op o
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | VectorCreate(dir, num) -> string_of_dir dir ^ " " ^ string_of_expr num
  | MatrixCreate(_) -> "TODO"
  | MatrixAccess(id, x, y) ->
    id ^ " [" ^ string_of_int x ^ ", " ^ string_of_int y ^ "]"
  | StructCreate(_) -> "TODO"
  | StructAccess(id1, id2) -> id1 ^ "." ^ id2
  | DupleCreate(x, y) -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

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
  | String -> "string"
  | Vector -> "vector"
  | Matrix -> "matrix"
  | Duple -> "duple"
  | StructT l -> l

let string_of_struct_d (id, t) = id ^ " : " ^ string_of_typ t ^ ";\n"

let string_of_vdecl = function
    Bind(t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | StructDef(id, l) -> "struct " ^ id ^ " {" ^ String.concat "" (List.map string_of_struct_d l) ^ "};\n"

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "\n" (List.map string_of_stmt fdecl.body) ^
  "\n"

