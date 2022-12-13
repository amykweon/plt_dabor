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
    and duple_t    = L.struct_type context [| (L.i32_type context); (L.i32_type context) |]
    in
  
  (* given type, generate size *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String -> string_t
    | A.Duple -> duple_t
    | _ -> void_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
  (* add struct definition *)
    let global_var m decls =
    match decls with
        A.Bind (t, n) -> 
          let init = match t with
            A.String -> L.const_pointer_null (ltype_of_typ t)
          | A.Duple -> L.const_struct context ([| (L.i32_type context); (L.i32_type context) |])
          | _ -> L.const_int (ltype_of_typ t) 0
          StringMap.add n (L.define_global n init the_module) m
      | _ -> m
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
    let main_type = L.function_type i32_t (Array.of_list []) in
    let the_main = L.define_function "main" main_type the_module in
    let builder = L.builder_at_end context (L.entry_block the_main) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let lookup n = StringMap.find n global_vars in
    
    (* IdRule implementation *)
    let build_idrule builder ((_, i): sid_typ) = match i with
        SId s     -> L.build_load (lookup s) s builder
      | _ -> raise (Failure "TODO")
    in

    let rec build_expr builder ((_, e) : sexpr) = match e with
          SIntLit i  -> L.const_int i32_t i
        | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
        | SStringLit s -> L.build_global_stringptr s "tmp" builder
        | SBinop(e1, op, e2) ->
            let e1' = build_expr builder e1
            and e2' = build_expr builder e2 in
            (match op with
            	  A.Add     -> L.build_add
	            | A.Sub     -> L.build_sub
	            | A.Multi    -> L.build_mul
              | A.Divide     -> L.build_sdiv
	            | A.And     -> L.build_and
	            | A.Or      -> L.build_or
	            | A.Equal   -> L.build_icmp L.Icmp.Eq
	            | A.Neq     -> L.build_icmp L.Icmp.Ne
	            | A.Less    -> L.build_icmp L.Icmp.Slt
	            | A.EqLess     -> L.build_icmp L.Icmp.Sle
	            | A.Greater -> L.build_icmp L.Icmp.Sgt
	            | A.EqGreater     -> L.build_icmp L.Icmp.Sge
              | A.Not     -> raise (Failure "NOT is a unary operator")
              | A.Mod     -> raise (Failure "Not implemented")
              | A.Move    -> raise (Failure "Not implemented")
            ) e1' e2' "tmp" builder
        | SUnop(op, e) ->
            let e' = build_expr builder e in
            (match op with
                A.Not -> L.build_not
              | _ -> raise (Failure "No other unary operation supported than NOT")
            )e' "tmp" builder
        | SPrintInt (e) -> L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
	          "printf" builder
        | SAssign (((_, i)), e) -> 
          (*
          let id_n = build_idrule builder id_t in
          *)
          let id_n = match i with
              SId s -> (lookup s)
            | _ -> raise (Failure ("TODO: ot implemented yet"))
            in
          let e' = build_expr builder e in
          ignore(L.build_store e' id_n builder); e'
        (*
        | SCall ("print", [e]) | SCall ("printb", [e]) ->
	          L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
	          "printf" builder
        | SAssign (id_t, e) -> raise (Failure "TODO")
        | SMatrixCreate (int_list) -> raise (Failure "TODO")
        | SStructCreate (s, s_l) -> raise (Failure "TODO")
        | SDupleCreate (i1, i2) -> raise (Failure "TODO")
        | SVectorCreate (dir, e) -> raise (Failure "TODO")
        *)
        | SIdRule id_t -> build_idrule builder id_t
        | _ -> raise (Failure "TODO")
      in
          
    let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder) in

    let rec build_stmt builder = function
          SBlock sl -> List.fold_left build_stmt builder sl
        | SExpr e -> ignore(build_expr builder e); builder
        | SIf (predicate, then_stmt, else_stmt) ->
            let bool_val = build_expr builder predicate in
            let merge_bb = L.append_block context "merge" the_main in
            let build_br_merge = L.build_br merge_bb in (* partial function *)

            let then_bb = L.append_block context "then" the_main in
            add_terminal (build_stmt (L.builder_at_end context then_bb) then_stmt)
            build_br_merge;

            let else_bb = L.append_block context "else" the_main in
            add_terminal (build_stmt (L.builder_at_end context else_bb) else_stmt)
            build_br_merge;

            ignore(L.build_cond_br bool_val then_bb else_bb builder);
            L.builder_at_end context merge_bb
        | SWhile (predicate, body) ->
            let pred_bb = L.append_block context "while" the_main in
            ignore(L.build_br pred_bb builder);

            let body_bb = L.append_block context "while_body" the_main in
            add_terminal (build_stmt (L.builder_at_end context body_bb) body)
            (L.build_br pred_bb);

            let pred_builder = L.builder_at_end context pred_bb in
            let bool_val = build_expr pred_builder predicate in

            let merge_bb = L.append_block context "merge" the_main in
            ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
            L.builder_at_end context merge_bb
        in

      let main_builder = build_stmt builder (SBlock stmts) in

      (* Add a return if the last block falls off the end *)
      add_terminal main_builder (L.build_ret (L.const_int i32_t 0))
  
  in
  
  build_main_body stmts;
  the_module
