module Vector = struct
  exception DimensionError of string   
  type vector = float list

  let custom_rev lst =
    let rec custom_rev_rec temp lst = 
      match lst with
      | [] -> temp
      | h :: t -> custom_rev_rec (h :: temp) t
    in
    custom_rev_rec [] lst
      
  let create n x = 
    if n >= 1 then 
      let rec create_rec n temp =
        if n <= 0 then 
          temp
        else
          create_rec (n - 1) (x :: temp)
      in
      create_rec n []
    else
      raise (DimensionError "Dimension should be greater than zero")
        
  let dim (v: vector) = 
    let rec dim_rec v cum =
      match v with
      | [] -> cum
      | _ :: t -> dim_rec t (cum + 1)      (* tail recursive *)
    in 
    dim_rec v 0

  let is_zero (v: vector) =
    if v = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else
      let rec is_zero_rec v =
        match v with
        | [] -> true
        | h :: t -> if (abs_float h > 1e-6) then false
                    else is_zero_rec t
      in
      is_zero_rec v
        
  let unit n j: vector =
    if 1 <= j && j <= n then
      let rec unit_rec i temp = 
        if i > n then temp
        else if i = j then unit_rec (i + 1) (1.0 :: temp)
        else unit_rec (i + 1) (0.0 :: temp)
      in 
      custom_rev (unit_rec 1 [])
    else 
      raise (DimensionError "Invalid position")
        
  let scale c v = 
    if v = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else
      let rec scale_rec v temp = 
        match v with 
        | [] -> custom_rev temp
        | h :: t -> scale_rec t ((c *. h) :: temp)
      in
      scale_rec v []
        
  let addv v1 v2 =
    if v1 = [] || v2 = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else if dim v1 <> dim v2 then
      raise (DimensionError "Mismatched Dimensions")
    else
      let rec add_rec v1 v2 temp =
        match (v1, v2) with
        | ([], []) -> custom_rev temp
        | (h1 :: t1, h2 :: t2) -> add_rec t1 t2 ((h1 +. h2) :: temp)
        | _ -> raise (DimensionError "Mismatched Dimensions")
      in add_rec v1 v2 []

  let subv v1 v2 =
    if v1 = [] || v2 = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else if dim v1 <> dim v2 then
      raise (DimensionError "Mismatched Dimensions")
    else
      let rec sub_rec v1 v2 temp =
        match (v1, v2) with
        | ([], []) -> custom_rev temp
        | (h1 :: t1, h2 :: t2) -> sub_rec t1 t2 ((h1 -. h2) :: temp)
        | _ -> raise (DimensionError "Mismatched Dimensions")
      in sub_rec v1 v2 []
    
  let dot_prod v1 v2 =
    if v1 = [] || v2 = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else if dim v1 <> dim v2 then
      raise (DimensionError "Mismatched Dimensions")
    else
      let rec dot_product_rec v1 v2 cum = 
        match (v1, v2) with
        | ([], []) -> cum
        | (h1 :: t1, h2 :: t2) -> dot_product_rec t1 t2 (cum +. (h1 *. h2))
        | _ -> raise (DimensionError "Mismatched Dimensions")
      in
      dot_product_rec v1 v2 0.0 
        
  let inverse v = 
    if v = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else
      let rec inverse_rec v temp =
        match v with 
        | [] -> custom_rev temp
        | h :: t -> inverse_rec t ((-1.0) *. h :: temp)
      in
      inverse_rec v []
      
  let length v = 
    if v = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else
      sqrt (dot_prod v v) 
       
  let angle v1 v2 =
    if v1 = [] || v2 = [] then 
      raise (DimensionError "Dimension should be greater than zero")
    else if dim v1 <> dim v2 then
      raise (DimensionError "Mismatched Dimensions")
    else 
      let dot = dot_prod v1 v2 in 
      let len1 = length v1 in 
      let len2 = length v2 in
      if (abs_float len1 < 1e-6) || (abs_float len2 < 1e-6) then
        raise (DimensionError "Length of vector cannot be zero")
      else 
        let cos_value = dot /. (len1 *. len2) in
        let cos_vall =
          if cos_value > 1.0 -. 1e-6 then 1.0
          else if cos_value < -1.0 +. 1e-6 then -1.0
          else cos_value
        in
        acos cos_vall

  (* projection of vector v1 on v2 *)
  let projectv v1 v2 =
    if v1 = [] || v2 = [] then 
      raise (DimensionError "Dimension should be greater than zero") 
    else if dim v1 <> dim v2 then
      raise (DimensionError "Mismatched Dimensions") 
    else
      let dot = dot_prod v1 v2 in
      let len2 = dot_prod v2 v2 in
      if abs_float len2 < 1e-6 then
        raise (DimensionError "Length of vector cannot be zero")
      else
        let scale_factor = dot /. len2 in
        scale scale_factor v2
end;;

open Vector;;

type types = Bool    (* boolean *)
           | Scalar   (* a scalar — any float value *)
           | Vector of int   (* n-dimensional with elements of type float*) ;;

type expr =  
    T | F   (* Boolean constants *)
  | ConstS of float    (* Scalar constants *)
  | ConstV of float list    (* Vector constants *)
  | Add of expr * expr   (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors of the same dimension *)
  | Inv of expr     (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
  | ScalProd of expr * expr   (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector *)
  | DotProd of expr * expr  (* dot product of two vectors of the same dimension *)
  | Mag of expr   (* overloaded: absolute value of a scalar or magnitude of a vector *)
  | Angle of expr * expr  (* in radians, the angle between two vectors *)
  | IsZero of expr (* overloaded: checks if a boolean expression evaluates to F,  or if a given scalar is within epsilon of 0.0 or is the vector close — within epsilon on each coordinate —  to the zero vector *)
  | Cond of expr * expr * expr  (* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)
;;

type values = B of bool | S of float | V of vector
                
exception Wrong of expr;;

let rec type_of e = match e with
  | ConstS _ -> Scalar
  | T -> Bool
  | F -> Bool 
  | ConstV v -> if (dim v) > 0 then Vector (dim v) else raise (Wrong e)
  | Add (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Scalar, Scalar) -> Scalar
       | (Bool, Bool) -> Bool
       | (Vector s1, Vector s2) -> if (s1 = s2 && s1 > 0) then Vector s1 else raise (Wrong e)
       | _ -> raise (Wrong e)) 
  | Inv e1 ->
      (match type_of e1 with 
       | Scalar -> Scalar
       | Bool -> Bool
       | Vector n -> if n > 0 then Vector n else raise (Wrong e))
  | ScalProd (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Scalar, Scalar) -> Scalar
       | (Bool, Bool) -> Bool
       | (Scalar, Vector s) -> if s > 0 then Vector s else raise (Wrong e)
       | (Vector s, Scalar) -> if s > 0 then Vector s else raise (Wrong e)
       | _ -> raise (Wrong e)) 
  | Angle (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Vector s1, Vector s2) -> if (s1 = s2 && s1 > 0) then Scalar else raise (Wrong e)
       | _ -> raise (Wrong e))
  | Mag e1 -> 
      (match type_of e1 with
       | Scalar -> Scalar
       | Vector n -> if n > 0 then Scalar else raise (Wrong e)
       | _ -> raise (Wrong e))
  | DotProd (e1, e2) ->
      (match (type_of e1, type_of e2) with
       | (Vector s1, Vector s2) -> if (s1 = s2 && s1 > 0) then Scalar else raise (Wrong e)
       | _ -> raise (Wrong e)) 
  | IsZero e1 ->
      (match type_of e1 with
       | Scalar -> Bool
       | Bool -> Bool
       | Vector n -> if n > 0 then Bool else raise (Wrong e))
  | Cond (e1, e2, e3) ->
      if type_of e1 = Bool then
        (match (type_of e2, type_of e3) with
         | (Scalar, Scalar) -> Scalar
         | (Bool, Bool) -> Bool
         | (Vector s1, Vector s2) -> 
             if (s1 = s2 && s1 > 0) then 
               Vector s1
             else raise (Wrong e)
         | _ -> raise (Wrong e))
      else raise (Wrong e)
;; 

let rec eval e = match e with
  | ConstS f -> S f
  | T -> B true 
  | F -> B false
  | ConstV v -> if (dim v) > 0 then V v else raise (Wrong e)
  | Add (e1, e2) ->
      (match (eval e1, eval e2) with 
       | S f1, S f2 -> S (f1 +. f2)
       | B b1, B b2 -> B (b1 || b2)
       | V v1, V v2 -> if (dim v1 = dim v2 && (dim v1) > 0) then V (addv v1 v2) 
                       else raise (Wrong e)
       | _ -> raise (Wrong e))
  | Inv e1 ->
      (match eval e1 with
       | S f -> S (-. f)
       | B b -> B (not b)
       | V v -> if (dim v) > 0 then V (inverse v) else raise (Wrong e))
  | ScalProd (e1, e2) ->
      (match (eval e1, eval e2) with
       | S f1, S f2 -> S (f1 *. f2)
       | B b1, B b2 -> B (b1 && b2)
       | S f, V v -> if (dim v) > 0 then V (scale f v) else raise (Wrong e)
       | V v, S f -> if (dim v) > 0 then V (scale f v) else raise (Wrong e) 
       | _ -> raise (Wrong e)) 
  | DotProd (e1, e2) ->
      (match (eval e1, eval e2) with
       | V v1, V v2 -> if (dim v1 = dim v2 && (dim v1) > 0) then S (dot_prod v1 v2) else raise (Wrong e)
       | _ -> raise (Wrong e))
  | Mag e1 ->
      (match eval e1 with
       | S f -> if f < 0.0 then S (-. f) else S f
       | V v -> if (dim v) > 0 then S (length v) else raise (Wrong e)
       | _ -> raise (Wrong e)) 
  | IsZero e1 ->
      (match eval e1 with
       | S f -> if (abs_float f < 1e-6) then B false else B true
       | B b ->
           (match b with 
            | true -> B false
            | false -> B true)
       | V v -> B (is_zero v)) 
  | Angle (e1, e2) ->
      (match (eval e1, eval e2) with
       | V v1, V v2 -> if (dim v1 = dim v2 && (dim v1) > 0) then S (angle v1 v2) else raise (Wrong e)
       | _ -> raise (Wrong e))
  | Cond (e1, e2, e3) ->  (* check e2 e3 types also *)
      let tempe1 = eval e1 in 
      if tempe1 = B true then 
        (match (eval e2, eval e3) with
         | S f1, S _ -> S f1
         | B b1, B _ -> B b1
         | V v1, V v2 -> if (dim v1 = dim v2 && (dim v1) > 0) then V v1 else raise (Wrong e)
         | _ -> raise (Wrong e))
      else if tempe1 = B false then
        (match (eval e2, eval e3) with
         | S _, S f2 -> S f2
         | B _, B b2 -> B b2
         | V v1, V v2 -> if (dim v1 = dim v2 && (dim v1) > 0) then V v2 else raise (Wrong e)
         | _ -> raise (Wrong e))
      else raise (Wrong e)
;;
