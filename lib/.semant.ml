open Ast
open Sast

module StringMap = Map.Make(String)
let check (vdecls, stmts) = 

  (* Verify a list of bindings has no duplicate names *)
  (* Also need to make sure structs are declared correctly too? *)
  let check_binds (kind : string) (binds : decl list) =
    (* let check_struct_def = function (*checks if struct elements are declared correctly *)*)
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds) (*might need to change this implementation*)
  in

  check_binds "global" vdecls;

  let struct_handler map element = 
    match element with
    | StructDef (name, field_list) -> StringMap.add name field_list map
    | _ -> map
  in
  let struct_field_info = List.fold_left struct_handler StringMap.empty vdecls in

  (* think about if we need to check if struct types are defined *)
  let symbol_handler map element = 
    match element with
    | Bind(ty, name) -> StringMap.add name ty map
    | _ -> map
  in
  let symbols = List.fold_left symbol_handler StringMap.empty vdecls in

  let type_of_identifier s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

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
  
  let rec check_expr = function
        IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | VectorCreate(dir, num) -> let snum = check_expr num in (Vector, SVectorCreate(dir, snum))
      | DupleCreate(i, j) -> (Duple, SDupleCreate(i, j))
      | MatrixCreate(els) -> (Matrix, SMatrixCreate(els))
      | MatrixAccess(var, i, j) -> 
        let lt = type_of_identifier var in
        let err = "trying to use non-matrix variable as matrix" in
        (check_assign lt Int err, SMatrixAccess(var, i, j))
      | MatrixAccessDup(m_name, d_name) ->
        let mt = type_of_identifier m_name in
        let dt = type_of_identifier d_name in
          if (mt = Matrix && dt = Duple) then
            (Int, SMatrixAccessDup(m_name, d_name))
          else
            raise (Failure ("tried to use matrix duple indexing with invalid types"))
      | MatrixAccessStruct(m_name, s_name, f_name) -> (
        let mt = type_of_identifier m_name in
        if (mt = Matrix) then
          let s_info = get_struct_info s_name in
            let f_type = get_field_type f_name s_info in
              match f_type with
              | Some Duple -> (Int, SMatrixAccessStruct(m_name, s_name, f_name))
              | Some _ -> raise (Failure ("trying to access matrix with non duple struct field"))
              | None -> raise (Failure ("struct doesn't have field matching given name"))
        else
          raise (Failure ("trying to use non-matrix variable as matrix"))
      )
      | StructCreate(name, fields) -> (
        let check_struct_object_creation ((s_f_name, s_f_type)) ((o_f_name, o_f_expr)) =
          let (o_f_type, _) = check_expr o_f_expr in
          s_f_name = o_f_name && s_f_type = o_f_type
        in
        let s_info = get_struct_info name in
        let correct_entries = List.map2 check_struct_object_creation s_info fields in
        let x = List.fold_left (fun x y -> x && y) true correct_entries in
          match x with 
            | true -> (StructT(name), SStructCreate(name, fields))
            | _ -> raise (Failure ("struct object fields don't match struct type"))
      )
      | StructAccess(name, field_name) -> (
        let s_info = get_struct_info name in
        let f_type = get_field_type field_name s_info in
          match f_type with
          | Some f_ty -> (f_ty, SStructAccess(name, field_name))
          | None -> raise (Failure ("struct doesn't have field matching given name"))
      )
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))
      (* | StructAssign() -> ()
      | MatrixAssign(m_name, ) -> ( *)

      )
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub when (t1 = Int) -> Int
            | Add | Sub when (t1 = Vector) -> Vector
            | Equal | Neq -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Unop(op, e1) -> let (t1, e1') = check_expr e1 in (t1, SUnop (op, (t1, e1')))
    in

    
        

(* need to implement functionality for matrix, vector, struct, if, and while *)

  
  
  (* let rec check_expr =  *)