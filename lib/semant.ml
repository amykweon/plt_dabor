open Ast
open Sast

module StringMap = Map.Make(String)
let check (program: program) = 
  let vdecls = program.locals in
  let stmts = program.body in
  let struct_handler map element = 
    match element with
    | StructDef (name, field_list) -> StringMap.add name field_list map
    | _ -> map
  in
  let struct_field_info = List.fold_left struct_handler StringMap.empty vdecls in

  (* think about if we need to check if struct types are defined *)
  let symbol_handler map element = 
    match element with
    | Bind(ty, name) -> (
      let dup = StringMap.find_opt name map in match dup with
        | Some _ -> raise (Failure ("duplicate variable name: " ^ name))
        | None -> StringMap.add name ty map
    )
    | _ -> map
  in
  let symbols = List.fold_left symbol_handler StringMap.empty vdecls in
  (* let matrix_info = StringMap.empty in *)

  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  (* (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in *)

  let get_struct_info s_name =
    try StringMap.find s_name struct_field_info
    with Not_found -> raise (Failure ("undeclared struct type " ^ s_name))
  in

  let rec get_field_type target_field_name s_info = 
    match s_info with
    | [] -> None
    | ((f_name,f_type) :: _ ) when f_name = target_field_name -> Some(f_type)
    | (_ :: tl) -> get_field_type target_field_name tl
  in
  
  let rec check_id_typ = function
        Id var -> (type_of_identifier var, SId var)
      | StructAccess(var_name, field_name) -> (
        let name_type = type_of_identifier var_name in
          match name_type with
          | StructT s_type -> let s_info = get_struct_info s_type in 
            let f_type = get_field_type field_name s_info in (
              match f_type with
              | Some f_ty -> (f_ty, SStructAccess(var_name, field_name))
              | None -> raise (Failure ("struct doesn't have field matching given name"))
          )
          | _ -> raise (Failure ("trying to use a non struct variable as a struct"))
      )
      | DupleAccess (var, i) -> 
        let vt = type_of_identifier var in
          if (vt = Duple) then
            (Int, SDupleAccess(var, i))
          else
            raise (Failure ("tried to use variable indexing with invalid types"))
      | IndexAccess(var, i, j) -> 
        let err = "trying to use a non matrix variable for index" in
        let lt = type_of_identifier var in
        let return = match lt with
            Matrix (_, _) -> (Int, SIndexAccess(var, i, j))
          | _ -> raise (Failure err)
        in return
      | IndexAccessVar(v_name, index) -> (
        let (id_ty, index') = check_id_typ index in
        let vt = type_of_identifier v_name in 
        let return = match vt with
            Matrix (_, _) -> if (id_ty = Duple) then (Int, SIndexAccessVar(v_name, (id_ty, index')))
              else raise (Failure ("tried to use variable indexing with invalid types"))
          | _ -> raise (Failure ("tried to use variable indexing with invalid types"))
        in return
      )
  in
  
  let rec check_expr = function
        IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | IdRule i -> let (typ, sx) = check_id_typ i in (typ, SIdRule(typ, sx))
      | VectorCreate(dir, num) -> let snum = check_expr num in (Vector, SVectorCreate(dir, snum))
      | DupleCreate(e1, e2) -> let i = check_expr e1 in let j = check_expr e2 in (Duple, SDupleCreate(i, j))
      | MatrixCreate(els) -> 
        let col = List.length els in
        let row = (List.length (List.nth els 0)) in
        let good = List.for_all (fun l -> (List.length l) = (List.length (List.nth els 0))) els in
        if good then 
          (Matrix(row, col), SMatrixCreate(els))
        else raise (Failure ("tried to init matrix with differing row lengths"))
      | StructCreate(struct_name, fields) -> (
        let check_struct_object_creation ((s_f_name, s_f_type)) ((o_f_name, o_f_expr)) =
          let (o_f_type, _) = check_expr o_f_expr in
          s_f_name = o_f_name && s_f_type = o_f_type
        in
        let s_info = get_struct_info struct_name in
        let correct_entries = List.map2 check_struct_object_creation s_info fields in
        let x = List.fold_left (fun x y -> x && y) true correct_entries in
          match x with 
            | true -> 
              let sfields = List.map (fun (name, field) -> let sfield = check_expr field in (name, sfield)) fields in
              (StructT(struct_name), SStructCreate(struct_name, sfields))
            | _ -> raise (Failure ("struct object fields don't match struct type"))
      )
      | Assign(id, e) -> 
        let (r_ty, _) as s_expr = check_expr e in
        let (id_typ, _) as s_id = check_id_typ id in
        if (id_typ = r_ty) then
          (* match name, expr' with
            | SId m_name, SMatrixCreate matrix -> 
              let matrix_info = StringMap.add m_name (List.length matrix, List.length (List.nth matrix 0)) matrix_info in
              (id_typ, SAssign(s_id, s_expr))
            | _ -> (id_typ, SAssign(s_id, s_expr)) *)
          (id_typ, SAssign(s_id, s_expr))
        else raise (Failure ("illegal assignment, types don't match up, LHS type: " ^ string_of_typ id_typ ^ " RHS type: " ^ string_of_typ r_ty))
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* Most binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub | Multi | Mod when (t1 = Int) -> Int
            | Add | Sub when (t1 = Vector) -> Vector
            | Equal | Neq -> Bool
            | Less | EqLess | Greater | EqGreater when t1 = Int -> Bool
            | And | Or | Not when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else (* Below covers special cases where binary operator is used between different types *)
          if (t1 = Duple && t2 = Vector) then
            match op with
              Move -> (Duple, SBinop((t1, e1'), op, (t2, e2')))
            | _ -> raise (Failure err)
          else if ((t1 = Vector && t2 = Int) || (t2 = Vector && t1 = Int)) then
            match op with
              Multi | Mod -> (Vector, SBinop((t1, e1'), op, (t2, e2')))
            | _ -> raise (Failure err)
          else  
            raise (Failure err)
      | Unop(op, e1) -> let (t1, e1') = check_expr e1 in (t1, SUnop (op, (t1, e1')))
      | PrintInt(e) -> 
          let (r_ty, _) as s_expr = check_expr e in
          if r_ty = Int then 
          (Int, SPrintInt(s_expr))
          else raise (Failure ("Cannot print data type other than int"))
      in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)

    in (* body of check_func *)
    (vdecls, check_stmt_list stmts)