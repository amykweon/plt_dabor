(* Code generation for dabor *)

module L = Llvm
module A = Ast
module S = Sast
module E = Semant
open Ast
open Llvm
open Sast

module StringMap = Map.Make(String)

let translate (globals, stmts) = 
    let context = L.global_context () in
    let the_module = L.create_module context "dabor" in
    
    let i32_t  = L.i32_type context
    and i8_t   = L.i8_type context
    and i1_t   = L.i1_type context
    and string_t = (L.pointer_type (L.i8_type context)) in
    
let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String-> L.pointer_type i8_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in
 
let rec ltype_of_typ = (function
      A.DataT(t) -> ltype_of_primitive t
    | A.StringT -> string_t
    | A.ArrayT(t,_) -> L.pointer_type (ltype_of_typ t)
    | A.RefT(_,t) ->  L.pointer_type (ltype_of_typ t)
    | A.StructT(t) -> (try let t = snd (StringHash.find structMap t) in L.pointer_type t with Not_found -> raise(Failure(t)))
    | _ -> void_t)  in

let lookup n = try StringMap.find n global_vars in

let build_stmt_body stmts =
    let builder = L.builder_at_end context (L.entry_block stmts) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

let rec build_expr builder ((_, e) : sexpr) = match e with
      SLiteral i  -> L.const_int i32_t i
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    | SBinop(e1, op, e2) as e ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
            | A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mult    -> L.build_fmul
            | A.Div     -> L.build_fdiv 
            | A.Mod     -> L.build_fmod
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Less    -> L.build_fcmp L.Fcmp.Olt
            | A.EqLess  -> L.build_fcmp L.Fcmp.Oleq
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | A.EqGreater     -> L.build_fcmp L.Fcmp.Ogeq
          ) e1' e2' "tmp" builder
    | SUnop(op, e) ->
          let e' = expr builder e in
            (match op with
            | A.Neg     -> L.build_fneq 
            | A.Neg     -> L.build_neq
            | A.Not     -> L.build_not
            ) e' "tmp" builder
    in
	    
let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

let rec stmt builder = function
      SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	     let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

     let then_bb = L.append_block context "then" the_function in
     add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
       build_br_merge;

     let else_bb = L.append_block context "else" the_function in
     add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
       build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb
      
    in

  List.iter build_stmt_body stmts;
  the_module