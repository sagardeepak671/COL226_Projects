open a2

(* Type Checking Tests *)
let%test "type_of (Add (ConstS (-1.5), ConstS 2.5))" = (type_of (Add (ConstS (-1.5), ConstS 2.5)) = Scalar)
let%test "type_of (Add (ConstV [0.0; (-2.0); 3.0], ConstV [1.0; 2.0; 3.0]))" = (type_of (Add (ConstV [0.0; -2.0; 3.0], ConstV [1.0; 2.0; 3.0])) = Vector 3)
let%test "type_of (Inv (F))" = (type_of (Inv (F)) = Bool)
let%test "type_of (IsZero (ConstS 5.0))" = (type_of (IsZero (ConstS 5.0)) = Bool)
let%test "type_of (ScalProd (ConstS (-2.0), ConstV [4.0; (-1.0); 3.0]))" = (type_of (ScalProd (ConstS (-2.0), ConstV [4.0; -1.0; 3.0])) = Vector 3)
let%test "type_of (DotProd (ConstV [3.0; (-2.0); 0.0], ConstV [1.0; 4.0; (-1.0)]))" = (type_of (DotProd (ConstV [3.0; -2.0; 0.0], ConstV [1.0; 4.0; -1.0])) = Scalar)
let%test "type_of (Mag (ConstV [-3.0; -4.0]))" = (type_of (Mag (ConstV [-3.0; -4.0])) = Scalar)
let%test "type_of (Cond (T, ConstS 4.0, ConstS (-1.5)))" = (type_of (Cond (T, ConstS 4.0, ConstS (-1.5))) = Scalar)
let%test "type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* Exception Cases for Type Checking *)
let%test "type_of (Add (ConstS 1.0, ConstV [1.0; 2.0])) raises Wrong" =
  (try let _ = type_of (Add (ConstS 1.0, ConstV [1.0; 2.0])) in false with | Wrong e -> e = Add (ConstS 1.0, ConstV [1.0; 2.0]) | _ -> false)

let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0; 2.0; 3.0])) raises Wrong" =
  (try let _ = type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0; 2.0; 3.0])) in false with | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [1.0; 2.0; 3.0]) | _ -> false)

(* Evaluation Tests *)
let%test "eval (Add (ConstS 2.0, ConstS 3.0))" = (eval (Add (ConstS 2.0, ConstS 3.0)) = S 5.0)
let%test "eval (Add (T, T))" = (eval (Add (T, T)) = B true)
let%test "eval (ScalProd (ConstS 0.0, ConstV [1.0; (-2.0); 3.0]))" = (eval (ScalProd (ConstS 0.0, ConstV [1.0; -2.0; 3.0])) = V [0.0; 0.0; 0.0])
let%test "eval (DotProd (ConstV [1.0; 0.0; (-1.0)], ConstV [(-1.0); 2.0; 1.0]))" = (eval (DotProd (ConstV [1.0; 0.0; -1.0], ConstV [-1.0; 2.0; 1.0])) = S (-2.0))
let%test "eval (Mag (ConstV [0.0; 0.0; 0.0]))" = (eval (Mag (ConstV [0.0; 0.0; 0.0])) = S 0.0)
let%test "eval (Cond (F, ConstS 3.0, ConstS 4.0))" = (eval (Cond (F, ConstS 3.0, ConstS 4.0)) = S 4.0)
let%test "eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [1.0; 2.0])

(* Exception Cases for Evaluation *)
let%test "eval (Add (ConstS 1.0, T)) raises Wrong" =
  (try let _ = eval (Add (ConstS 1.0, T)) in false with | Wrong e -> e = Add (ConstS 1.0, T) | _ -> false)

let%test "eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [1.0])) raises Wrong" =
  (try let _ = eval (DotProd (ConstV [1.0; 2.0; 3.0], ConstV [1.0])) in false with | Wrong e -> e = DotProd (ConstV [1.0; 2.0; 3.0], ConstV [1.0]) | _ -> false)

let%test "eval (Cond (F, ConstS 1.0, T)) raises Wrong" =
  (try let _ = eval (Cond (F, ConstS 1.0, T)) in false with | Wrong e -> e = Cond (F, ConstS 1.0, T) | _ -> false)



(*more*) 
(* Type Checking Tests *)
let%test "type_of (Add (ConstS (-1.5), ConstS (2.5)))" = 
  (type_of (Add (ConstS (-1.5), ConstS (2.5))) = Scalar)

let%test "type_of (Add (ConstV ([0.0; -2.0; 3.0]), ConstV ([1.0; 2.0; 3.0])))" = 
  (type_of (Add (ConstV ([0.0; -2.0; 3.0]), ConstV ([1.0; 2.0; 3.0]))) = Vector (3))

let%test "type_of (Cond (T, ConstV ([1.0; 2.0]), ConstV ([3.0; 4.0])))" = 
  (type_of (Cond (T, ConstV ([1.0; 2.0]), ConstV ([3.0; 4.0]))) = Vector (2))

(* Large Vector Cases *)
let%test "type_of (Add (ConstV ([1.0; 2.0; 3.0; 4.0; 5.0; 6.0]), ConstV ([6.0; 5.0; 4.0; 3.0; 2.0; 1.0])))" = 
  (type_of (Add (ConstV ([1.0; 2.0; 3.0; 4.0; 5.0; 6.0]), ConstV ([6.0; 5.0; 4.0; 3.0; 2.0; 1.0]))) = Vector (6))

(* Exception Cases for Type Checking *)
let%test "type_of (Add (ConstS (1.0), ConstV ([1.0; 2.0]))) raises Wrong" =
  (try 
    let _ = type_of (Add (ConstS (1.0), ConstV ([1.0; 2.0]))) in false 
  with 
  | Wrong e -> e = Add (ConstS (1.0), ConstV ([1.0; 2.0])) 
  | _ -> false)

(* Evaluation Tests *)
let%test "eval (Add (ConstS (2.0), ConstS (3.0)))" = 
  (eval (Add (ConstS (2.0), ConstS (3.0))) = S (5.0))

let%test "eval (ScalProd (ConstS (-2.0), ConstV ([4.0; -1.0; 3.0])))" = 
  (eval (ScalProd (ConstS (-2.0), ConstV ([4.0; -1.0; 3.0]))) = V ([-8.0; 2.0; -6.0]))

let%test "eval (DotProd (ConstV ([1.0; 2.0; 3.0]), ConstV ([4.0; 5.0; 6.0])))" = 
  (eval (DotProd (ConstV ([1.0; 2.0; 3.0]), ConstV ([4.0; 5.0; 6.0]))) = S (32.0))

let%test "eval (Mag (ConstV ([3.0; 4.0])))" = 
  (eval (Mag (ConstV ([3.0; 4.0]))) = S (5.0))

let%test "eval (Cond (T, ConstS (4.0), ConstS (-1.5)))" = 
  (eval (Cond (T, ConstS (4.0), ConstS (-1.5))) = S (4.0))

(* Large Vector Operations *)
let%test "eval (Add (ConstV ([1.0; 2.0; 3.0; 4.0; 5.0; 6.0]), ConstV ([6.0; 5.0; 4.0; 3.0; 2.0; 1.0])))" = 
  (eval (Add (ConstV ([1.0; 2.0; 3.0; 4.0; 5.0; 6.0]), ConstV ([6.0; 5.0; 4.0; 3.0; 2.0; 1.0]))) = V ([7.0; 7.0; 7.0; 7.0; 7.0; 7.0]))

let%test "eval (DotProd (ConstV ([1.0; 2.0; 3.0; 4.0; 5.0; 6.0]), ConstV ([6.0; 5.0; 4.0; 3.0; 2.0; 1.0])))" = 
  (eval (DotProd (ConstV ([1.0; 2.0; 3.0; 4.0; 5.0; 6.0]), ConstV ([6.0; 5.0; 4.0; 3.0; 2.0; 1.0]))) = S (56.0))

(* Exception Cases for Evaluation *)
let%test "eval (Add (ConstS (1.0), T)) raises Wrong" =
  (try 
    let _ = eval (Add (ConstS (1.0), T)) in false 
  with 
  | Wrong e -> e = Add (ConstS (1.0), T) 
  | _ -> false)

let%test "eval (DotProd (ConstV ([1.0; 2.0; 3.0]), ConstV ([1.0]))) raises Wrong" =
  (try 
    let _ = eval (DotProd (ConstV ([1.0; 2.0; 3.0]), ConstV ([1.0]))) in false 
  with 
  | Wrong e -> e = DotProd (ConstV ([1.0; 2.0; 3.0]), ConstV ([1.0])) 
  | _ -> false)

let%test "eval (Cond (F, ConstS (1.0), T)) raises Wrong" =
  (try 
    let _ = eval (Cond (F, ConstS (1.0), T)) in false 
  with 
  | Wrong e -> e = Cond (F, ConstS (1.0), T) 
  | _ -> false)
 

let%test "eval (ScalProd (T, ConstV ([1.0; 2.0]))) raises Wrong" =
  (try 
    let _ = eval (ScalProd (T, ConstV ([1.0; 2.0]))) in false 
  with 
  | Wrong e -> e = ScalProd (T, ConstV ([1.0; 2.0])) 
  | _ -> false)

let%test "eval (Add (ConstV ([1.0; 2.0; 3.0]), ConstV ([1.0; 2.0]))) raises Wrong" =
  (try 
    let _ = eval (Add (ConstV ([1.0; 2.0; 3.0]), ConstV ([1.0; 2.0]))) in false 
  with 
  | Wrong e -> e = Add (ConstV ([1.0; 2.0; 3.0]), ConstV ([1.0; 2.0])) 
  | _ -> false)


(*more complex*)
(* Type Checking Tests *)
let%test "type_of (Add (ConstS (-1.5), ConstS 2.5))" = (type_of (Add (ConstS (-1.5), ConstS 2.5)) = Scalar)
let%test "type_of (Add (ConstV [0.0; (-2.0); 3.0], ConstV [1.0; 2.0; 3.0]))" = (type_of (Add (ConstV [0.0; -2.0; 3.0], ConstV [1.0; 2.0; 3.0])) = Vector 3)
let%test "type_of (Inv (F))" = (type_of (Inv (F)) = Bool)
let%test "type_of (IsZero (ConstS 5.0))" = (type_of (IsZero (ConstS 5.0)) = Bool)
let%test "type_of (ScalProd (ConstS (-2.0), ConstV [4.0; (-1.0); 3.0]))" = (type_of (ScalProd (ConstS (-2.0), ConstV [4.0; -1.0; 3.0])) = Vector 3)
let%test "type_of (DotProd (ConstV [3.0; (-2.0); 0.0], ConstV [1.0; 4.0; (-1.0)]))" = (type_of (DotProd (ConstV [3.0; -2.0; 0.0], ConstV [1.0; 4.0; -1.0])) = Scalar)
let%test "type_of (Mag (ConstV [-3.0; -4.0]))" = (type_of (Mag (ConstV [-3.0; -4.0])) = Scalar)
let%test "type_of (Cond (T, ConstS 4.0, ConstS (-1.5)))" = (type_of (Cond (T, ConstS 4.0, ConstS (-1.5))) = Scalar)
let%test "type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* Exception Cases for Type Checking *)
let%test "type_of (Add (ConstS 1.0, ConstV [1.0; 2.0])) raises Wrong" =
  (try let _ = type_of (Add (ConstS 1.0, ConstV [1.0; 2.0])) in false with | Wrong e -> e = Add (ConstS 1.0, ConstV [1.0; 2.0]) | _ -> false)


(* Evaluation Tests *)
let%test "eval (Add (ConstS 2.0, ConstS 3.0))" = (eval (Add (ConstS 2.0, ConstS 3.0)) = S 5.0)
let%test "eval (Add (T, T))" = (eval (Add (T, T)) = B true)
let%test "eval (ScalProd (ConstS 0.0, ConstV [1.0, -2.0, 3.0]))" = (eval (ScalProd (ConstS 0.0, ConstV [1.0; -2.0; 3.0])) = V [0.0; 0.0; 0.0])
let%test "eval (Mag (ConstV [0.0, 0.0, 0.0]))" = (eval (Mag (ConstV [0.0; 0.0; 0.0])) = S 0.0)
let%test "eval (Cond (F, ConstS 3.0, ConstS 4.0))" = (eval (Cond (F, ConstS 3.0, ConstS 4.0)) = S 4.0)
let%test "eval (Cond (T, ConstV [1.0, 2.0], ConstV [3.0, 4.0]))" = (eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [1.0; 2.0])

(* Exception Cases for Evaluation *)
let%test "eval (Add (ConstS 1.0, T)) raises Wrong" =
  (try let _ = eval (Add (ConstS 1.0, T)) in false with | Wrong e -> e = Add (ConstS 1.0, T) | _ -> false)

let%test "eval (DotProd (ConstV [1.0, 2.0], ConstV [1.0, 2.0, 3.0])) raises Wrong" =
  (try let _ = eval (DotProd (ConstV [1.0; 2.0], ConstV [1.0; 2.0; 3.0])) in false with | Wrong e -> e = DotProd (ConstV [1.0;2.0], ConstV [1.0; 2.0; 3.0]) | _ -> false)

let%test "eval (Cond (F, ConstS 1.0, T)) raises Wrong" =
  (try let _ = eval (Cond (F, ConstS 1.0, T)) in false with | Wrong e -> e = Cond (F, ConstS 1.0, T) | _ -> false)



(*big test cases*)
(* Type Checking Tests *)
let%test "type_of (Add (ConstS (-1.5), ConstS 2.5))" = (type_of (Add (ConstS (-1.5), ConstS 2.5)) = Scalar)
let%test "type_of (Add (ConstV [0.0; (-2.0); 3.0], ConstV [1.0; 2.0; 3.0]))" = (type_of (Add (ConstV [0.0; -2.0; 3.0], ConstV [1.0; 2.0; 3.0])) = Vector 3)
let%test "type_of (Inv (F))" = (type_of (Inv (F)) = Bool)
let%test "type_of (IsZero (ConstS 5.0))" = (type_of (IsZero (ConstS 5.0)) = Bool)
let%test "type_of (ScalProd (ConstS (-2.0), ConstV [4.0; (-1.0); 3.0]))" = (type_of (ScalProd (ConstS (-2.0), ConstV [4.0; -1.0; 3.0])) = Vector 3)
let%test "type_of (DotProd (ConstV [3.0; (-2.0); 0.0], ConstV [1.0; 4.0; (-1.0)]))" = (type_of (DotProd (ConstV [3.0; -2.0; 0.0], ConstV [1.0; 4.0; -1.0])) = Scalar)
let%test "type_of (Mag (ConstV [-3.0; -4.0]))" = (type_of (Mag (ConstV [-3.0; -4.0])) = Scalar)
let%test "type_of (Cond (T, ConstS 4.0, ConstS (-1.5)))" = (type_of (Cond (T, ConstS 4.0, ConstS (-1.5))) = Scalar)
let%test "type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0]))" = (type_of (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = Vector 2)

(* Exception Cases for Type Checking *)
let%test "type_of (Add (ConstS 1.0, ConstV [1.0; 2.0])) raises Wrong" =
  (try let _ = type_of (Add (ConstS 1.0, ConstV [1.0; 2.0])) in false with | Wrong e -> e = Add (ConstS 1.0, ConstV [1.0; 2.0]) | _ -> false)

let%test "type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0; 2.0; 3.0])) raises Wrong" =
  (try let _ = type_of (DotProd (ConstV [1.0; 2.0], ConstV [1.0; 2.0; 3.0])) in false with | Wrong e -> e = DotProd (ConstV [1.0; 2.0], ConstV [1.0; 2.0; 3.0]) | _ -> false)

(* Evaluation Tests *)
let%test "eval (Add (ConstS 2.0, ConstS 3.0))" = (eval (Add (ConstS 2.0, ConstS 3.0)) = S 5.0)
let%test "eval (Add (T, T))" = (eval (Add (T, T)) = B true)
let%test "eval (ScalProd (ConstS 0.0, ConstV [1.0, -2.0, 3.0]))" = (eval (ScalProd (ConstS 0.0, ConstV [1.0; -2.0; 3.0])) = V [0.0; 0.0; 0.0]) 
let%test "eval (Mag (ConstV [0.0, 0.0, 0.0]))" = (eval (Mag (ConstV [0.0; 0.0; 0.0])) = S 0.0)
let%test "eval (Cond (F, ConstS 3.0, ConstS 4.0))" = (eval (Cond (F, ConstS 3.0, ConstS 4.0)) = S 4.0)
let%test "eval (Cond (T, ConstV [1.0, 2.0], ConstV [3.0, 4.0]))" = (eval (Cond (T, ConstV [1.0; 2.0], ConstV [3.0; 4.0])) = V [1.0; 2.0])

(* Exception Cases for Evaluation *)
let%test "eval (Add (ConstS 1.0, T)) raises Wrong" =
  (try let _ = eval (Add (ConstS 1.0, T)) in false with | Wrong e -> e = Add (ConstS 1.0, T) | _ -> false)

let%test "eval (DotProd (ConstV [1.0, 2.0], ConstV [1.0])) raises Wrong" =
  (try let _ = eval (DotProd (ConstV [1.0; 2.0], ConstV [1.0])) in false with | Wrong e -> e = DotProd (ConstV [1.0;2.0], ConstV [1.0]) | _ -> false)


(* raises Wrong test cases for empty vectors and other expressions mixeed*)
(* Type Checking Tests *)
let%test "type_of (Add (ConstV [], ConstV [])) raises Wrong" =
  (try let _ = type_of (Add (ConstV [], ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

let%test "type_of (ScalProd (ConstS 0.0, ConstV [])) raises Wrong" =
  (try let _ = type_of (ScalProd (ConstS 0.0, ConstV [])) in false with | Wrong e -> e =(ConstV []) | _ -> false)

let%test "type_of (DotProd (ConstV [], ConstV [])) raises Wrong" =
  (try let _ = type_of (DotProd (ConstV [], ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

let%test "type_of (Mag (ConstV [])) raises Wrong" =
  (try let _ = type_of (Mag (ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

let%test "type_of (Cond (T, ConstV [], ConstV [])) raises Wrong" =
  (try let _ = type_of (Cond (T, ConstV [], ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

let%test "type_of (Add (ConstS 1.0, ConstV [])) raises Wrong" =
  (try let _ = type_of (Add (ConstS 1.0, ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

(* Evaluation Tests *)
let%test "eval (Add (ConstV [], ConstV [])) raises Wrong"
  = (try let _ = eval (Add (ConstV [], ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

let%test "eval (ScalProd (ConstS 0.0, ConstV [])) raises Wrong"
  = (try let _ = eval (ScalProd (ConstS 0.0, ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

let%test "eval (DotProd (ConstV [], ConstV [])) raises Wrong"
  = (try let _ = eval (DotProd (ConstV [], ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

(*mixing emppty and nonemty*)
let%test "eval (Add (ConstV [1.0; 2.0], ConstV [])) raises Wrong"
  = (try let _ = eval (Add (ConstV [1.0; 2.0], ConstV [])) in false with | Wrong e -> e = (ConstV []) | _ -> false)

