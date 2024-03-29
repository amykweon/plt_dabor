(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mod | Multi | Equal | Neq | Less | EqLess | Greater | EqGreater | And | Or | Move | Not | Neg

type typ = Int | Bool | String | Vector | Matrix of int * int | StructT of string | Duple

type id_typ = 
    Id of string
  | StructAccess of string * string
  | DupleAccess of string * int
  | IndexAccess of string * int * int
  | IndexAccessVar of string * id_typ

type expr =
    IntLit of int
  | BoolLit of bool
  | StringLit of string
  | IdRule of id_typ 
  | VectorCreate of expr * expr
  | Binop of expr * op * expr
  | Unop of op * expr
  | Assign of id_typ * expr
  | MatrixCreate of (int list) list
  | StructCreate of string * ((string * expr) list)
  | DupleCreate of expr * expr
  | PrintInt of expr
  | PrintStr of expr
  | PrintMat of id_typ
  | PrintDup of expr
  | PrintVec of expr

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
   body: stmt list;
}

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mod -> "%"
  | Multi -> "*"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | EqLess -> "<="
  | Greater -> ">"
  | EqGreater -> ">="
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Neg -> "!"
  | Move -> "move" 

let string_of_matrix (i) = string_of_int i ^ " "
let string_of_matrix_l (l) = "[ " ^ String.concat "" (List.map string_of_matrix l) ^ "]\n"

let rec string_id_typ = function
    Id(s) -> s
  | StructAccess(id1, id2) -> id1 ^ "." ^ id2
  | DupleAccess (id, x) -> id ^ "[" ^ string_of_int x ^ "]"
  | IndexAccess(id, x, y) ->
    id ^ " [" ^ string_of_int x ^ ", " ^ string_of_int y ^ "]"
  | IndexAccessVar (id, v) -> id ^ " [" ^ string_id_typ v ^ "]"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | IdRule(v) -> string_id_typ v
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) ->  string_of_op o ^ " " ^ string_of_expr e
  | Assign(v, e) -> string_id_typ v ^ " = " ^ string_of_expr e
  | VectorCreate(x, y) -> "<" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ ">"
  | MatrixCreate(l) -> "[" ^ String.concat "" (List.map string_of_matrix_l l) ^ "]"
  | StructCreate(id, l) -> id ^ " {" ^ String.concat "" (List.map struct_of_struct_e l) ^ "}"
  | DupleCreate(x, y) -> "(" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ ")"
  | PrintInt(e) -> "print integer: " ^ string_of_expr e
  | PrintStr(e) -> "print string: " ^ string_of_expr e
  | PrintMat(e) -> "print matrix: " ^ string_id_typ e
  | PrintDup(e) -> "print duple: " ^ string_of_expr e
  | PrintVec(e) -> "print vector: " ^ string_of_expr e
and struct_of_struct_e (id, e) = id ^ " : " ^ string_of_expr e ^ ";\n"

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
  | Matrix(i, j) -> "matrix [" ^ string_of_int i ^ ", " ^ string_of_int j ^ "]"
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