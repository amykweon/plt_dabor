open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SVectorCreate of dir * sexpr
  | SBinop of sexpr * op * sexpr
  | SUnop of op * sexpr
  (* | SAssign of string * sexpr *)
  | SAssign of id_typ * sexpr
  | SMatrixCreate of int list list
  | SMatrixAccess of string * int * int
  | SMatrixAccessDup of string * string
  | SMatrixAccessStruct of string * string * string
  | SStructCreate of string * ((string * expr) list)
  | SStructAccess of string * string
  | SDupleCreate of int * int

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt

type sprogram = {
   locals: decl list;
   body: sstmt list;
}

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
      SIntLit(l) -> string_of_int l
    | SBoolLit(true) -> "true"
    | SBoolLit(false) -> "false"
    | SStringLit(l) -> l
    | SId(s) -> s
    | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SUnop(o, e) -> string_of_sexpr e ^ " " ^ string_of_op o
    | SAssign(_, _) -> " " (*v ^ " = " ^ string_of_sexpr e*)
    | SVectorCreate(dir, num) -> string_of_dir dir ^ " " ^ string_of_sexpr num
    | SMatrixCreate(_) -> "TODO"
    | SMatrixAccess(id, x, y) ->
      id ^ " [" ^ string_of_int x ^ ", " ^ string_of_int y ^ "]"
    | SMatrixAccessDup(id1, id2) -> id1 ^ "[" ^ id2 ^ "]"
    | SMatrixAccessStruct(id1, id2, id3) -> id1 ^ "[" ^ id2 ^ "." ^ id3 ^ "]"
    | SStructCreate(_) -> "TODO"
    | SStructAccess(id1, id2) -> id1 ^ "." ^ id2
    | SDupleCreate(x, y) -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sprogram fdecl =
  "\n\n Sementically Checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "\n" (List.map string_of_sstmt fdecl.body) ^
  "\n"
