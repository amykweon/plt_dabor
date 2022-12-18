(* Code generation for dabor *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
let translate (struct_field_info, globals, stmts) = 
  let context = L.global_context () in
  let the_module = L.create_module context "dabor" in
  
  let i32_t  = L.i32_type context
  and i8_t   = L.i8_type context
  and i1_t   = L.i1_type context
  and string_t = (L.pointer_type (L.i8_type context))
  and vector_t = L.struct_type context [| (L.pointer_type (L.i8_type context)); (L.i32_type context) |]
  and array_t = L.array_type
  in

  let structPtrTyps = Hashtbl.create 10 in (* No longer really pointer types *)
  let structNames = Hashtbl.create 10 in (* Mapping from struct object name to struct type name*)

  let get_field_idx s_name f_name = 
    let s_info = StringMap.find s_name struct_field_info in
    let rec find f_name_target lst =
      match lst with
      [] -> raise( Failure ("Named struct field not found"))
    | (f_name, _) :: tl -> if f_name = f_name_target then 0 else 1 + find f_name_target tl
    in
    find f_name s_info
  in 

  (* given type, generate size *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String -> string_t
    | A.Duple -> array_t i32_t 2
    | A.Matrix(r,c) -> array_t (array_t i32_t c) r
    | A.Vector -> vector_t
    | A.StructT(s_name) -> 
      let s_ptr_type = Hashtbl.find_opt structPtrTyps s_name in 
        ( match s_ptr_type with 
        | Some (s_ptr_type) -> s_ptr_type
        | None -> let s_ptr_type = L.named_struct_type context s_name in
                  Hashtbl.add structPtrTyps s_name s_ptr_type; (* Hashtable is mutable *)
                  L.struct_set_body s_ptr_type (Array.of_list (List.map ltype_of_typ (List.map snd (StringMap.find s_name struct_field_info)))) false; 
                  s_ptr_type
        )
  in


(* let rec iter_struct_create = function
          | [] -> []
          | (f_name, f_expr) :: tl -> 
            let e_ptr = build_expr builder f_expr in
            let f_idx = get_field_idx s_name f_name in
            let ll_f_idx = L.const_int i32_t f_idx in
            let f_ptr = L.build_gep s_val [|ll_f_idx|] f_name builder in
            llstore e_ptr f_ptr builder;
            iter_struct_create tl
          in
          ignore(print_endline "About to do struct stuff");
          ignore(iter_struct_create field_init_list); *)

  let createStructFields (_, f_typ) = 
      match f_typ with
       A.Duple -> (L.const_pointer_null (L.pointer_type i32_t))
      | _ -> (L.const_pointer_null (ltype_of_typ f_typ))
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
  (* add struct definition *)
    let global_var m decls =
    match decls with
        A.Bind (t, n) -> 
          let init = match t with
            A.String -> L.const_pointer_null (ltype_of_typ t)
          | A.Duple -> L.const_pointer_null (L.pointer_type i32_t)
          | A.Matrix(r, c) -> L.const_array (array_t i32_t c) (Array.make r (L.const_int i32_t 0))
          | A.Vector -> L.const_pointer_null (L.pointer_type (ltype_of_typ t))
          | A.StructT(s_name) -> 
              Hashtbl.add structNames n s_name;
              let s_fields = StringMap.find s_name struct_field_info  in
              L.const_named_struct (ltype_of_typ t) (Array.of_list (List.map createStructFields s_fields))
          | _ -> L.const_int (ltype_of_typ t) 0
        in
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


  let llstore lval laddr builder =
    let ptr = L.build_pointercast laddr (L.pointer_type (L.type_of lval)) "" builder in
    let store_inst = (L.build_store lval ptr builder) in
    ignore ((L.string_of_llvalue store_inst));
    ()
  in

  (* let llload ltyp laddr builder =
    let ptr = L.build_pointercast laddr (L.pointer_type ltyp) "" builder in
    let store_inst = (L.build_load ptr "" builder) in
    store_inst
    (* ignore ((L.string_of_llvalue store_inst));
    () *)
  in *)

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

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and string_format_str =  L.build_global_stringptr "%s\n" "fmt" builder
    and duple_format_str = L.build_global_stringptr "(%d, %d)\n" "fmt" builder
    and vector_format_str = L.build_global_stringptr "%s (%d)\n" "fmt" builder
    in

    let lookup n = StringMap.find n global_vars in
    
    (* IdRule implementation *)
    let rec build_idrule builder ((_, i): sid_typ) = match i with
        SId s     -> L.build_load (lookup s) s builder
      | SDupleAccess (v, index) ->
        let i' = L.const_int i32_t index in
        let ptr_load = 
          let ptr = lookup v in
          L.build_load ptr v builder in
        let ptr_gep = L.build_in_bounds_gep ptr_load [|i'|] v builder in
          L.build_load ptr_gep v builder
      | SIndexAccess (id, i, j) ->
          let i' = L.const_int i32_t i in
          let j' = L.const_int i32_t j in
          let ptr = lookup id in
          let ptr_gep = L.build_gep ptr [|L.const_int i32_t 0; i'; j'|] id builder in
            L.build_load ptr_gep id builder
      | SIndexAccessVar (id, var) ->
          let v' = build_idrule builder var in
          let v'_iptr = L.build_in_bounds_gep v' [|L.const_int i32_t 0|] "" builder in
          let i' = L.build_load v'_iptr "i" builder in
          let v'_jptr = L.build_in_bounds_gep v' [|L.const_int i32_t 1|] "" builder in
          let j' = L.build_load v'_jptr "j" builder in
          let ptr = lookup id in
          let ptr_gep = L.build_gep ptr [|L.const_int i32_t 0; i'; j'|] id builder in
            L.build_load ptr_gep id builder
      | SStructAccess(id, f_name) ->
          let s_name = Hashtbl.find structNames id in
          let f_idx = get_field_idx s_name f_name in
          let i' = L.const_int i32_t f_idx in
          let s_ptr = lookup id in
          let ptr_gep = L.build_gep s_ptr [|L.const_int i32_t 0; i'|] id builder in
          (* let ltyp = ltype_of_typ tp in
          llload ltyp ptr_gep builder *)
          L.build_load ptr_gep id builder

    and build_expr builder ((_, e) : sexpr) = match e with
          SIntLit i  -> L.const_int i32_t i
        | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
        | SStringLit s -> L.build_global_stringptr s "tmp" builder
        | SBinop((t1, e_1), op, (t2, e_2)) ->
          let e1' = build_expr builder (t1, e_1)
          and e2' = build_expr builder (t2, e_2) in
          if (t1 = t2 && (t1 = A.Int || t1 = A.String)) then
            (match op with
            	  A.Add     -> L.build_add
	            | A.Sub     -> L.build_sub
	            | A.Multi    -> L.build_mul
              | A.Mod     -> L.build_sdiv
	            | A.And     -> L.build_and
	            | A.Or      -> L.build_or
	            | A.Equal   -> L.build_icmp L.Icmp.Eq
	            | A.Neq     -> L.build_icmp L.Icmp.Ne
	            | A.Less    -> L.build_icmp L.Icmp.Slt
	            | A.EqLess     -> L.build_icmp L.Icmp.Sle
	            | A.Greater -> L.build_icmp L.Icmp.Sgt
	            | A.EqGreater     -> L.build_icmp L.Icmp.Sge
              | A.Not     -> raise (Failure "NOT is a unary operator")
              | A.Neg     -> raise (Failure "NEG is a unary operator")
              | _         -> raise (Failure ("illegal binary operator"))
          ) e1' e2' "tmp" builder
          else if ((t1 = A.Vector && t2 = A.Int)) then
            let compute = match op with
	              A.Multi   ->
                  let ptr_gep = L.build_struct_gep e1' 1 "" builder in
                  let int_r =
                    let i_load = L.build_load ptr_gep "" builder in
                    L.build_mul i_load e2' "tmp" builder in
                  ignore(L.build_store int_r ptr_gep builder); e1'
              | A.Mod     -> 
                  let ptr_gep = L.build_struct_gep e1' 1 "" builder in
                  let int_r =
                    let i_load = L.build_load ptr_gep "" builder in
                    L.build_sdiv i_load e2' "tmp" builder in
                  ignore(L.build_store int_r ptr_gep builder); e1'
              | _         -> raise (Failure ("illegal binary operator"))
            in compute
          else if ((t2 = A.Vector && t1 = A.Int)) then
            let compute = match op with
                A.Multi   ->
                  let ptr_gep = L.build_struct_gep e2' 1 "" builder in
                  let int_r =
                    let i_load = L.build_load ptr_gep "" builder in
                    L.build_mul i_load e1' "tmp" builder in
                  ignore(L.build_store int_r ptr_gep builder); e2'
              | A.Mod     -> 
                  let ptr_gep = L.build_struct_gep e2' 1 "" builder in
                  let int_r =
                    let i_load = L.build_load ptr_gep "" builder in
                    L.build_sdiv i_load e1' "tmp" builder in
                  ignore(L.build_store int_r ptr_gep builder); e2'
              | _         -> raise (Failure ("illegal binary operator"))
            in compute
          else if ((t1 = t2 ) && (t1 = A.Vector)) then
            let compute = match op with
            	  A.Add     -> raise (Failure ("vector operation not implemented"))
	            | A.Sub     -> raise (Failure ("vector operation not implemented"))
              | _         -> raise (Failure ("illegal binary operator"))
            in compute
          else if ((t1 = A.Duple && t2 = A.Vector)) then
            let compute = match op with
                A.Move   ->
                  let dr_gep = L.build_in_bounds_gep e1' [|L.const_int i32_t 0|] "" builder in
                  let dr = L.build_load dr_gep "" builder in
                  let dc_gep = L.build_in_bounds_gep e1' [|L.const_int i32_t 1|] "" builder in
                  let dc = L.build_load dc_gep "" builder in

                  let int_v =
                    let int_gep = L.build_struct_gep e2' 1 "" builder in 
                    L.build_load int_gep "" builder in
                  let dir_v = 
                    let dir_gep = L.build_struct_gep e2' 0 "" builder in 
                    L.build_load dir_gep "" builder in
                  
                  let diagR = L.build_global_stringptr "DiagR" "tmp" builder 
                  and diagL = L.build_global_stringptr "DiagL" "tmp" builder 
                  and hori = L.build_global_stringptr "Hori" "tmp" builder in
                  (* and vert = L.build_global_stringptr "Vert" "tmp" builder in *)
                  let result_r =
                      if (dir_v = diagR) then L.build_add dr int_v "tmp" builder
                      else if (dir_v = diagL) then L.build_add dr int_v "tmp" builder
                      else if (dir_v = hori) then L.build_add dr (L.const_int i32_t 0) "tmp" builder
                      else L.build_add dr int_v "tmp" builder
                    in
                    let result_c =
                      if (dir_v = diagR) then L.build_add dc int_v "tmp" builder
                      else if (dir_v = diagL) then L.build_sub dc int_v "tmp" builder
                      else if (dir_v = hori) then L.build_add dc int_v "tmp" builder
                      else L.build_add dc (L.const_int i32_t 0) "tmp" builder
                    in ignore(L.build_store result_c dc_gep builder);
                  ignore(L.build_store result_r dr_gep builder); e1'
              | _         -> raise (Failure ("illegal binary operator"))
            in compute
          else raise (Failure ("illegal binary operation"))
        | SUnop(op, e) ->
            let e' = build_expr builder e in
            (match op with
                A.Not -> L.build_not
              | A.Neg -> L.build_neg
              | _ -> raise (Failure "No other unary operation supported than NOT and NEG")
            )e' "tmp" builder
        | SPrintInt (e) -> L.build_call printf_func [| int_format_str ; (build_expr builder e) |] "printf" builder
        | SPrintStr (e) -> L.build_call printf_func [| string_format_str ; (build_expr builder e) |] "printf" builder
        | SPrintMat (id) -> let print_inst = match id with
            A.Matrix(r, c), SId id' ->
              let m = lookup id' in
              let (print_str, print_array) =
                let rec row_print r' c' str array = 
                  if c' < c then 
                    let ptr_gep = L.build_gep m [|L.const_int i32_t 0 ; L.const_int i32_t r'; L.const_int i32_t c'|] "" builder in
                    let ele = L.build_load ptr_gep "" builder in 
                    row_print r' (c' + 1) (str ^ "%d ") (Array.append array [| ele |])
                  else (str, array)
                in
                let rec col_print r' c' str array =
                  if r' < r then
                    let (str_array, row_array) = row_print r' c' str array in col_print (r'+1) 0 (str_array ^ "\n") row_array
                  else (str, array)
                in col_print 0 0 "" [||]
              in L.build_call printf_func (Array.append [|(L.build_global_stringptr print_str "fmt" builder) |] print_array) "printf" builder
          | _ -> raise (Failure "print_matrix only supports matrix type")
          in print_inst
        | SPrintDup (id) -> 
            let dr_gep = L.build_in_bounds_gep (build_expr builder id) [|L.const_int i32_t 0|] "" builder in
            let dr = L.build_load dr_gep "" builder in
            let dc_gep = L.build_in_bounds_gep (build_expr builder id) [|L.const_int i32_t 1|] "" builder in
            let dc = L.build_load dc_gep "" builder in
            L.build_call printf_func [| duple_format_str ; dr ; dc|] "printf" builder
        | SPrintVec (id) -> 
              let int_gep = L.build_struct_gep (build_expr builder id) 1 "" builder in 
              let int_v = L.build_load int_gep "" builder in
              let dir_gep = L.build_struct_gep (build_expr builder id) 0 "" builder in 
              let dir_v = L.build_load dir_gep "" builder in
              L.build_call printf_func [| vector_format_str ; dir_v ; int_v |] "printf" builder
        | SAssign ((_, i), e) ->
          let add = match i with
              SId s -> let id_add = (lookup s) in
                let e' = build_expr builder e in
                ignore(llstore e' id_add builder); e'
                (* ignore(L.build_store e' id_add builder); e' *)
            | SDupleAccess (v, index) -> 
              let i' = L.const_int i32_t index in
              let e' = build_expr builder e in
              let ptr = lookup v in
              let ptr_load = L.build_load ptr v builder in
              let ptr_gep = L.build_in_bounds_gep ptr_load [|i'|] v builder in
                ignore(L.build_store e' ptr_gep builder); e'
            | SIndexAccess (id, i, j) ->
              let e' = build_expr builder e in
              let i' = L.const_int i32_t i in
              let j' = L.const_int i32_t j in
              let ptr = lookup id in
              let ptr_gep = L.build_gep ptr [|L.const_int i32_t 0; i'; j'|] id builder in
              ignore(L.build_store e' ptr_gep builder); e'
            | SIndexAccessVar (id, var) ->
              let e' = build_expr builder e in
              let v' = build_idrule builder var in
              let v'_iptr = L.build_in_bounds_gep v' [|L.const_int i32_t 0|] "" builder in
              let i' = L.build_load v'_iptr "i" builder in
              let v'_jptr = L.build_in_bounds_gep v' [|L.const_int i32_t 1|] "" builder in
              let j' = L.build_load v'_jptr "j" builder in
              let ptr = lookup id in
              let ptr_gep = L.build_gep ptr [|L.const_int i32_t 0; i'; j'|] id builder in
              ignore(L.build_store e' ptr_gep builder); e'
            | SStructAccess(id, f_name) ->
                let s_name = Hashtbl.find structNames id in
                let f_idx = get_field_idx s_name f_name in
                let i' = L.const_int i32_t f_idx in
                let e' = build_expr builder e in
                let s_ptr = lookup id in
                let ptr_gep = L.build_gep s_ptr [|L.const_int i32_t 0; i'|] id builder in
                ignore(llstore e' ptr_gep builder); e'
                (* ignore(L.build_store e' ptr_gep builder); e' *)
          in add
        (*
        | SVectorCreate (dir, e) -> raise (Failure "TODO")
        *)
        | SVectorCreate (dir, e) ->
          let e' = build_expr builder e in
          let vector' = L.build_alloca vector_t "" builder in
          let dir' = match dir with
              A.Hori -> L.build_global_stringptr "Hori" "tmp" builder
            | A.Vert -> L.build_global_stringptr "Vert" "tmp" builder
            | A.DiagL -> L.build_global_stringptr "DiagL" "tmp" builder
            | A.DiagR -> L.build_global_stringptr "DiagR" "tmp" builder
          in ignore (
            let eptr = L.build_struct_gep vector' 0 "" builder in llstore dir' eptr builder;
            let eptr = L.build_struct_gep vector' 1 "" builder in llstore e' eptr builder;
          ); vector'
        | SMatrixCreate (int_list) ->
          let lists       = List.map (List.map (L.const_int i32_t)) int_list in
          let innerArray   = List.map Array.of_list lists in
          let list2array  = Array.of_list ((List.map (L.const_array i32_t) innerArray)) in
            L.const_array (array_t i32_t (List.length (List.hd int_list))) list2array
        | SStructCreate (s_name, _) ->
          let s_fields = StringMap.find s_name struct_field_info  in
          L.const_named_struct (ltype_of_typ (A.StructT(s_name))) (Array.of_list (List.map createStructFields s_fields))
          (* L.const_named_struct (ltype_of_typ (A.StructT(s_name))) (Array.of_list (List.map (fun f -> L.const_pointer_null (ltype_of_typ (snd f))) s_fields)) *)
        | SDupleCreate (i1, i2) ->
          let int1 = build_expr builder i1 in
          let int2 = build_expr builder i2 in
          let duple_ptr = L.build_array_malloc i32_t (L.const_int i32_t 1) "" builder in
          ignore ( 
            let indx = L.const_int i32_t 0 in
            let eptr = L.build_gep duple_ptr [|indx|] "" builder in llstore int1 eptr builder;
            let indy = L.const_int i32_t 1 in
            let eptr = L.build_gep duple_ptr [|indy|] "" builder in llstore int2 eptr builder;
          ); (duple_ptr)
        | SIdRule id_t -> build_idrule builder id_t
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