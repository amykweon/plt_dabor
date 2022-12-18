open Ast

type sid_typ = typ * si
and si =
    SId of string
  | SStructAccess of string * string
  | SDupleAccess of string * int
  | SIndexAccess of string * int * int
  | SIndexAccessVar of string * sid_typ

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SBoolLit of bool
  | SStringLit of string
  | SIdRule of sid_typ
  | SVectorCreate of dir * sexpr
  | SBinop of sexpr * op * sexpr
  | SUnop of op * sexpr
  | SAssign of sid_typ * sexpr
  | SMatrixCreate of (int list) list
  | SStructCreate of string * ((string * sexpr) list)
  | SDupleCreate of sexpr * sexpr
  | SPrintInt of sexpr
  | SPrintStr of sexpr
  | SPrintMat of sid_typ

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
let rec string_sid_typ (t, i)=
"(" ^ string_of_typ t ^ " : " ^ (match i with
    SId(s) -> s
  | SDupleAccess (id, x) -> id ^ "[" ^ string_of_int x ^ "]"
  | SStructAccess(id1, id2) -> id1 ^ "." ^ id2
  | SIndexAccess(id, x, y) ->
    id ^ " [" ^ string_of_int x ^ ", " ^ string_of_int y ^ "]"
  | SIndexAccessVar(id, e) -> id ^ " [" ^ string_sid_typ e ^ "]"
 ) ^ ")"

let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
      SIntLit(l) -> string_of_int l
    | SBoolLit(true) -> "true"
    | SBoolLit(false) -> "false"
    | SStringLit(l) -> l
    | SIdRule(s) -> string_sid_typ s
    | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
    | SUnop(o, e) -> string_of_sexpr e ^ " " ^ string_of_op o
    | SAssign(v, e) -> string_sid_typ v ^ " = " ^ string_of_sexpr e
    | SVectorCreate(dir, num) -> string_of_dir dir ^ " " ^ string_of_sexpr num
    | SMatrixCreate(l) -> "[" ^ String.concat "" (List.map string_of_matrix_l l) ^ "]"
    | SStructCreate(id, l) -> id ^ " {" ^ String.concat "" (List.map struct_of_struct_se l) ^ "}"
    | SDupleCreate(x, y) -> "(" ^ string_of_sexpr x ^ ", " ^ string_of_sexpr y ^ ")"
    | SPrintInt(e) -> "print integer: " ^ string_of_sexpr e
    | SPrintStr(e) -> "print string: " ^ string_of_sexpr e
    | SPrintMat(e) -> "print matrix: " ^ string_sid_typ e
  )
  and struct_of_struct_se (id, e) = id ^ " : " ^ string_of_sexpr e ^ ";\n"
   ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sprogram (_, locals, body) =
  "\n\n Semantically Checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl locals) ^
  String.concat "\n" (List.map string_of_sstmt body) ^
  "\n"