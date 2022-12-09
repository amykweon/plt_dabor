(* Code generation for dabor *)

module L = Llvm
module A = Ast
module S = Sast
module E = Semant
open Ast
open Llvm
open Sast

module StringMap = Map.Make(String)

let translate (functions) = 
    let context = L.global_context () in
    let the_module = L.create_module context "dabor"
    and i32_t  = L.i32_type context
    and i8_t   = L.i8_type context
    and i1_t   = L.i1_type context
    and float_t = L.double_type context
    and string_t = (L.pointer_type (L.i8_type context))
    and void_t = L.void_type context in
    
let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.String-> L.pointer_type i8_t
    | A.Void  -> void_t in
    
let rec ltype_of_typ = (function
      A.DataT(t) -> ltype_of_primitive t
    | A.StringT -> string_t
    | A.ArrayT(t,_) -> L.pointer_type (ltype_of_typ t)
    | A.RefT(_,t) ->  L.pointer_type (ltype_of_typ t)
    | A.StructT(t) -> (try let t = snd (StringHash.find structMap t) in L.pointer_type t with Not_found -> raise(Failure(t)))
    | _ -> void_t)  in

let rec check_expr = function
    | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
    
        if t1 = t2 then
          let t = match op with
            | A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mult    -> L.build_fmul
            | A.Div     -> L.build_fdiv 
            | A.Mod     -> L.build_fmod
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Less    -> L.build_fcmp L.Fcmp.Olt
            | A.Leq     -> L.build_fcmp L.Fcmp.Oleq
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | A.Geq     -> L.build_fcmp L.Fcmp.Ogeq

    | SUnop(op, (t1, e1') ) ->
          let e' = expr builder e in
            (match op with
            | A.Neg     -> L.build_fneq 
            | A.Neg     -> L.build_neq
            | A.Not     -> L.build_not
            
let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
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
