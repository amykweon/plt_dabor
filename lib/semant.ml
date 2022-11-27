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
  
  let rec check_expr = function
        IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | VectorCreate(dir, num) -> let snum = check_expr num in (Vector, SVectorCreate(dir, snum))
      | MatrixCreate(els) -> (Matrix, SMatrixCreate(els))
      | MatrixAccess(var, i, j) -> 
        let lt = type_of_identifier var in
        let err = "trying to use non-matrix variable as matrix" in
        (check_assign lt Int err, SMatrixAccess(var, i, j))
      | StructCreate(name, fields) -> 
        let check_struct_object_creation s_info_entry field_entry = 
          match s_info_entry with
            (f_name, f_type) -> (
            match field_entry with
              (o_name, o_expr) -> let (o_type, _) = check_expr o_expr in
              f_name = o_name && f_type = o_type
            )
          false
        in
        let s_info = get_struct_info name in
        let correct_entries = List.map2 check_struct_object_creation s_info fields in
        


        
        
        ()
      | StructAccess(name, field) ->

        ()
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))

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
              Add | Sub when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    
        )

(* need to implement functionality for matrix, vector, struct, if, and while *)

  
  
  (* let rec check_expr =  *)