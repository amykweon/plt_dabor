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
  | SAssign of string * sexpr
  | SMatrixCreate of int list list
  | SMatrixAccess of string * int * int
  | SStructCreate of (string * sexpr) list
  | SStructAccess of string * string
  | SDupleCreate of int * int

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt

// TODO: bind to decl
type sprogram = {
   locals: bind list;
   body: sstmt list;
}

(* Pretty-printing functions *)
let rec string_of_sexpr = function
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sprogram (vars, funcs) =
  "\n\n Sementically Checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "\n" (List.map string_of_sstmt fdecl.body) ^
  "\n"
