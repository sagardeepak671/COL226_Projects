(* type_checker.ml *)
open Ast

(* hastable *)
let symbol_table : (string, types) Hashtbl.t = Hashtbl.create 100
let value_table : (string, expr) Hashtbl.t = Hashtbl.create 100

(* Exception  *)
exception Type_error of string
exception Undefined_variable of string
 
let string_of_type = function
  | Bool -> "Bool"
  | Scalar_Int -> "Scalar_Int"
  | Scalar_Float -> "Scalar_Float"
  | Vector_Int n -> "Vector_Int(" ^ string_of_int n ^ ")"
  | Vector_Float n -> "Vector_Float(" ^ string_of_int n ^ ")"
  | Matrix_Int (r, c) -> "Matrix_Int(" ^ string_of_int r ^ "," ^ string_of_int c ^ ")"
  | Matrix_Float (r, c) -> "Matrix_Float(" ^ string_of_int r ^ "," ^ string_of_int c ^ ")"
 (*dim of vector*)
let vector_dimension = function
  | ConstV v -> List.length v
  | ConstIntV v -> List.length v
  | _ -> raise (Type_error "Not a vector")

(* dim of matrix *)
let matrix_dimensions = function
  | ConstM m -> 
      let rows = List.length m in
      let cols = if rows = 0 then 0 else List.length (List.hd m) in
      (rows, cols)
  | ConstIntM m -> 
      let rows = List.length m in
      let cols = if rows = 0 then 0 else List.length (List.hd m) in
      (rows, cols)
  | _ -> raise (Type_error "Not a matrix")

(* Type checking *)
let rec type_of_expr = function
  | T | F -> Bool
  | ConstS _ -> Scalar_Float
  | ConstI _ -> Scalar_Int
  | ConstV v -> 
      if v = [] then raise (Type_error "Empty vector")
      else Vector_Float (List.length v)
  | ConstIntV v -> 
      if v = [] then raise (Type_error "Empty vector")
      else Vector_Int (List.length v)
  | ConstM m -> 
      if m = [] then raise (Type_error "Empty matrix")
      else Matrix_Float (List.length m, List.length (List.hd m))
  | ConstIntM m -> 
      if m = [] then raise (Type_error "Empty matrix")
      else Matrix_Int (List.length m, List.length (List.hd m))
  | Var v ->
      (try Hashtbl.find symbol_table v
       with Not_found -> raise (Undefined_variable ("Variable " ^ v ^ " not defined")))
  | Inv e -> type_of_expr e  (* Negation/inversion preserves type *)
  | Add (e1, e2) -> 
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      (match (t1, t2) with
       | (Bool, Bool) -> Bool
       | (Scalar_Float, Scalar_Float) -> Scalar_Float
       | (Scalar_Int, Scalar_Int) -> Scalar_Int
       | (Vector_Float n1, Vector_Float n2) when n1 = n2 -> Vector_Float n1
       | (Vector_Int n1, Vector_Int n2) when n1 = n2 -> Vector_Int n1
       | (Matrix_Float (r1, c1), Matrix_Float (r2, c2)) when r1 = r2 && c1 = c2 -> 
           Matrix_Float (r1, c1)
       | (Matrix_Int (r1, c1), Matrix_Int (r2, c2)) when r1 = r2 && c1 = c2 -> 
           Matrix_Int (r1, c1)
       | _ -> raise (Type_error ("Incompatible types for addition: " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2)))
  | ScalProd (e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      (match (t1, t2) with
       | (Scalar_Int, Scalar_Int) -> Scalar_Int
       | (Scalar_Float, Scalar_Float) -> Scalar_Float
       | (Scalar_Int, Vector_Int n) | (Vector_Int n, Scalar_Int) -> Vector_Int n
       | (Scalar_Float, Vector_Float n) | (Vector_Float n, Scalar_Float) -> Vector_Float n
       | (Scalar_Int, Matrix_Int(r,c)) | (Matrix_Int(r,c), Scalar_Int) -> Matrix_Int(r,c)
       | (Scalar_Float, Matrix_Float(r,c)) | (Matrix_Float(r,c), Scalar_Float) -> Matrix_Float(r,c)
       | _ -> raise (Type_error ("Incompatible types for scalar product: " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2)))
  | DotProd (e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      (match (t1, t2) with
       | (Vector_Int n1, Vector_Int n2) when n1 = n2 -> Scalar_Int
       | (Vector_Float n1, Vector_Float n2) when n1 = n2 -> Scalar_Float
       | _ -> raise (Type_error ("Incompatible types for dot product: " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2)))
  | Mag e ->
      let t = type_of_expr e in
      (match t with
       | Scalar_Int -> Scalar_Int
       | Scalar_Float -> Scalar_Float
       | Vector_Int _ -> Scalar_Int
       | Vector_Float _ -> Scalar_Float
       | _ -> raise (Type_error ("Invalid type for magnitude: " ^ string_of_type t)))
  | Angle (e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      (match (t1, t2) with
       | (Vector_Int n1, Vector_Int n2) | (Vector_Float n1, Vector_Float n2) 
         when n1 = n2 -> Scalar_Float
       | _ -> raise (Type_error ("Incompatible types for angle: " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2)))
  | IsZero _ -> Bool
  | Eq (_, _) | Neq (_, _) | Lt (_, _) | Gt (_, _) | Le (_, _) | Ge (_, _) -> Bool
  | Cond (e1, e2, e3) ->
      let t1 = type_of_expr e1 in
      if t1 <> Bool then
        raise (Type_error "Condition must be boolean")
      else
        let t2 = type_of_expr e2 in
        let t3 = type_of_expr e3 in
        if t2 = t3 then t2
        else raise (Type_error ("Branches of conditional have different types: " ^ 
                              string_of_type t2 ^ " and " ^ string_of_type t3))
  | Input _ | Print _ -> Scalar_Float
  | Transpose e ->
      let t = type_of_expr e in
      (match t with
       | Matrix_Int (r, c) -> Matrix_Int (c, r)
       | Matrix_Float (r, c) -> Matrix_Float (c, r)
       | _ -> raise (Type_error ("Cannot transpose non-matrix type: " ^ string_of_type t)))
  | Mult (e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      (match (t1, t2) with
       | (Matrix_Int (r1, c1), Matrix_Int (r2, c2)) when c1 = r2 -> Matrix_Int (r1, c2)
       | (Matrix_Float (r1, c1), Matrix_Float (r2, c2)) when c1 = r2 -> Matrix_Float (r1, c2)
       | _ -> raise (Type_error ("Incompatible types for matrix multiplication: " ^ 
                                string_of_type t1 ^ " and " ^ string_of_type t2)))
  | Det e ->
      let t = type_of_expr e in
      (match t with
       | Matrix_Int (r, c) when r = c -> Scalar_Int
       | Matrix_Float (r, c) when r = c -> Scalar_Float
       | _ -> raise (Type_error ("Cannot compute determinant of non-square matrix: " ^ string_of_type t)))
  | Inverse e ->
      let t = type_of_expr e in
      (match t with
       | Matrix_Int (r, c) when r = c -> Matrix_Int (r, c)
       | Matrix_Float (r, c) when r = c -> Matrix_Float (r, c)
       | _ -> raise (Type_error ("Cannot compute inverse of non-square matrix: " ^ string_of_type t)))
  | VectorDecl (_, size, e) ->
      (match e with
       | ConstV _ -> Vector_Float size
       | ConstIntV _ -> Vector_Int size
       | _ -> raise (Type_error "Vector declaration requires a vector literal"))
  | MatrixDecl (_, rows, cols, e) ->
      (match e with
       | ConstM _ -> Matrix_Float (rows, cols)
       | ConstIntM _ -> Matrix_Int (rows, cols)
       | _ -> raise (Type_error "Matrix declaration requires a matrix literal"))

(* Type check a statement *)
let rec typecheck_stmt = function
  | Assign (id, e) -> 
      let expr_type = type_of_expr e in
      Hashtbl.replace symbol_table id expr_type;
      Hashtbl.replace value_table id e
  | Seq stmts ->
      List.iter typecheck_stmt stmts
  | If (cond, tblock, fblock) ->
      if type_of_expr cond = Bool then
        (typecheck_stmt tblock; typecheck_stmt fblock)
      else raise (Type_error "Condition must be boolean")
  | While (cond, body) ->
      if type_of_expr cond = Bool then typecheck_stmt body
      else raise (Type_error "While condition must be boolean")
  | For (var, start, stop, step, body) ->
      (match type_of_expr start, type_of_expr stop, type_of_expr step with
       | Scalar_Int, Scalar_Int, Scalar_Int -> 
           Hashtbl.replace symbol_table var Scalar_Int;
           typecheck_stmt body
       | _ -> raise (Type_error "Loop bounds must be integers"))
  | PrintStmt _ -> ()
  | InputStmt _ -> ()
  | ExprStmt e -> 
      (match e with
       | VectorDecl(id, size, expr) ->
           (match expr with
            | ConstV _ -> 
                let vec_type = Vector_Float size in
                Hashtbl.replace symbol_table id vec_type;
                Hashtbl.replace value_table id expr
            | ConstIntV _ -> 
                let vec_type = Vector_Int size in
                Hashtbl.replace symbol_table id vec_type;
                Hashtbl.replace value_table id expr
            | _ -> raise (Type_error "Vector declaration requires a vector literal"))
       | MatrixDecl(id, rows, cols, expr) ->
           (match expr with
            | ConstM _ -> 
                let mat_type = Matrix_Float (rows, cols) in
                Hashtbl.replace symbol_table id mat_type;
                Hashtbl.replace value_table id expr
            | ConstIntM _ -> 
                let mat_type = Matrix_Int (rows, cols) in
                Hashtbl.replace symbol_table id mat_type;
                Hashtbl.replace value_table id expr
            | _ -> raise (Type_error "Matrix declaration requires a matrix literal"))
       | _ -> let _ = type_of_expr e in ())
  | VectorStmt (_, size, e) -> 
      (match e with
       | ConstV _ -> 
           let vec_type = Vector_Float size in
           let var = (match e with VectorDecl(id,_,_) -> id | _ -> "") in
           Hashtbl.replace symbol_table var vec_type;
           Hashtbl.replace value_table var e
       | ConstIntV _ -> 
           let vec_type = Vector_Int size in
           let var = (match e with VectorDecl(id,_,_) -> id | _ -> "") in
           Hashtbl.replace symbol_table var vec_type;
           Hashtbl.replace value_table var e
       | _ -> raise (Type_error "Vector assignment requires a vector literal"))
  | MatrixStmt (_, rows, cols, e) -> 
      (match e with
       | ConstM _ -> 
           let mat_type = Matrix_Float (rows, cols) in
           let var = (match e with MatrixDecl(id,_,_,_) -> id | _ -> "") in
           Hashtbl.replace symbol_table var mat_type;
           Hashtbl.replace value_table var e
       | ConstIntM _ -> 
           let mat_type = Matrix_Int (rows, cols) in
           let var = (match e with MatrixDecl(id,_,_,_) -> id | _ -> "") in
           Hashtbl.replace symbol_table var mat_type;
           Hashtbl.replace value_table var e
       | _ -> raise (Type_error "Matrix assignment requires a matrix literal"))
 
let typecheck_program prog = 
  try
    (match prog with
     | Seq stmts -> List.iter typecheck_stmt stmts
     | _ -> typecheck_stmt prog);
    true, "Type checking successful", symbol_table
  with
  | Type_error msg -> false, "Type error: " ^ msg, symbol_table
  | Undefined_variable msg -> false, msg, symbol_table
 
let print_env () =
  Printf.printf "\nSymbol Table Contents:\n";
  Hashtbl.iter (fun var typ -> 
    Printf.printf "  %s : %s\n" var (string_of_type typ)
  ) symbol_table
