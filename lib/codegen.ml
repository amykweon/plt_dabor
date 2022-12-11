(* Code generation for dabor *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (globals, stmts) = 
    let context = L.global_context () in
    let the_module = L.create_module context "dabor" in
    
    let i32_t  = L.i32_type context
    and i8_t   = L.i8_type context
    and i1_t   = L.i1_type context
    and string_t = (L.pointer_type (L.i8_type context))
    and void_t = L.void_type context
    in
  
  (* given type, generate size *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String-> string_t
    | _ -> void_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
  (* add struct definition *)
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0 in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty globals 
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in

(*
let rec ltype_of_typ = (function
      A.DataT(t) -> ltype_of_primitive t
    | A.StringT -> string_t
    | A.ArrayT(t,_) -> L.pointer_type (ltype_of_typ t)
    | A.RefT(_,t) ->  L.pointer_type (ltype_of_typ t)
    | A.StructT(t) -> (try let t = snd (StringHash.find structMap t) in L.pointer_type t with Not_found -> raise(Failure(t)))
    | _ -> void_t)  in
*)

  (* fill in the stmts *)
  let build_main_body stmts =
    let main_type = L.function_type void_t (Array.of_list [void_t]) in
    let the_main = L.define_function "main" main_type the_module in
    let builder = L.builder_at_end context (L.entry_block the_main) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    let lookup n = StringMap.find n global_vars in

    (* IdRule implementation
    let rec build_idrule builder ((_, i): sid_typ) = match i with
      ...
    in
    *)

    let rec build_expr builder ((_, e) : sexpr) = match e with
          SIntLit i  -> L.const_int i32_t i
        | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
        | SBinop(e1, op, e2) as e ->
            let e1' = build_expr builder e1
            and e2' = build_expr builder e2 in
            (match op with
                A.Add     -> L.build_fadd
              | A.Sub     -> L.build_fsub
              | A.Multi   -> L.build_fmul
              | A.Divide  -> L.build_fdiv 
              | A.Mod     -> L.build_fmod
              | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
              | A.Neq     -> L.build_fcmp L.Fcmp.One
              | A.Less    -> L.build_fcmp L.Fcmp.Olt
              | A.EqLess  -> L.build_fcmp L.Fcmp.Oeql
              | A.Greater -> L.build_fcmp L.Fcmp.Ogt
              | A.EqGreater     -> L.build_fcmp L.Fcmp.Oeqg
            ) e1' e2' "tmp" builder
        | SUnop(op, e) ->
            let e' = build_expr builder e in
            L.build_fneq e' "tmp" builder
        (*
        | SCall ("print", [e]) | SCall ("printb", [e]) ->
	          L.build_call printf_func [| int_format_str ; (expr builder e) |]
	          "printf" builder
        | SStringLit s -> L.build_global_stringptr s "tmp" builder
        | SIdRule id_t -> ignore("TODO")
        | SAssign (id_t, e) -> ignore("TODO")
        | SMatrixCreate (int_list) -> ignore("TODO")
        | SStructCreate (s, s_l) -> ignore("TODO")
        | SDupleCreate (i1, i2) -> ignore("TODO")
        | SVectorCreate (dir, e) -> ignore("TODO")
        *)
          
    let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder) in

    let rec build_stmt builder = function
          SBlock sl -> List.fold_left build_stmt builder sl
        | SExpr e -> ignore(build_expr builder e); builder
        | SIf (predicate, then_stmt, else_stmt) ->
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

      let main_builder = build_stmt builder (SBlock stmts) in

      (* Add a return if the last block falls off the end *)
      add_terminal main_builder (L.build_ret (L.const_int i32_t 0))
  
  in
  
  List.iter build_main_body stmts;
  the_module
