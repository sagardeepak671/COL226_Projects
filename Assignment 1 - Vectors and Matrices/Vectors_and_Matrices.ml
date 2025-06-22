(* CODE *)
module Vector = struct
  exception DimensionError of string   
  type vector = float list

  (*Reverse the list*)
  let rec custom_rev lst =
    let rec custom_rev_rec temp lst = 
      match lst with
      | [] -> temp
      | h :: t -> custom_rev_rec (h :: temp) t
    in
    custom_rev_rec [] lst

  (*Create the Vector*)
  let create n x = 
    if n>=1 then 
      let rec create_rec n temp=
        if n<=0 then 
          temp
        else
          create_rec (n-1) (x::temp)
      in
      create_rec n []
    else
      raise(DimensionError "Dimension should be greater than zero")
  
  (*Finding the Dimension*)
  let dim (v:vector) = 
    if v=[] then
      raise(DimensionError "Dimension should be greater than zero")
    else
      let rec dim_rec v cum=
        match v with
        |[]->cum
        |h::t-> dim_rec t (cum + 1)      (*tail recursive*)
      in 
      dim_rec v 0
                  
                               
  (*Check all are zeroes or not*)
  let is_zero (v:vector) =
    if v=[] then 
      raise(DimensionError "Dimension should be greater than zero")
    else
      let rec is_zero_rec v=
        match v with
        |[]->true
        |h::t->if h <> 0.0 then false
            else
              is_zero_rec t
      in
      is_zero_rec v
        
  (*Check unit vector*) 
  let unit n j:vector =
    if 1<=j && j<=n then
      let rec unit_rec i temp = 
        if i>n then temp
        else if i==j then unit_rec (i+1) (1.0::temp)
        else unit_rec (i+1) (0.0::temp)
      in 
      custom_rev (unit_rec 1 [])
    else 
      raise(DimensionError "Invalid position")
        
  (*Scale the vector by a scalar*)
  let scale c v = 
    if v=[] then 
      raise(DimensionError "Dimension should be greater than zero")
    else
      let rec scale_rec v temp = 
        match v with 
        |[]-> custom_rev temp
        |h::t->scale_rec t (( c *. h):: temp)
      in
      scale_rec v [] 
        
  (*adding the vectors*)
  let addv v1 v2=
    if v1=[]|| v2=[] then 
      raise(DimensionError "Dimension should be greater than zero")
    else if dim v1 <> dim v2 then
      raise(DimensionError "Mismatched Dimensions")
    else
      let rec add_rec v1 v2 temp =
        match (v1,v2) with
        |([],[])-> custom_rev temp
        |(h1::t1,h2::t2)->add_rec t1 t2 ((h1+. h2)::temp)
        | _ -> raise(DimensionError "Mismatched Dimensions")
      in add_rec v1 v2 []
  
  (*Dot product*)
  let dot_prod v1 v2=
    if v1=[]||v2=[] then 
      raise(DimensionError "Dimension should be greater than zero")
    else if dim v1 <> dim v2 then
      raise(DimensionError "Mismatched Dimensions")
    else
      let rec dot_product_rec v1 v2 cum = 
        match (v1,v2)with
        |([],[])->cum
        |(h1::t1,h2::t2)->dot_product_rec t1 t2 (cum +. (h1 *. h2))
        |_ -> raise(DimensionError "Mismatched Dimensions")
      in
      dot_product_rec v1 v2 0.0
 
  (*Finding inverse*)
  let inverse v = 
    if v=[] then 
      raise(DimensionError "Dimension should be greater than zero")
    else
      let rec inverse_rec v temp=
        match v with 
        |[]->custom_rev temp
        |h::t->inverse_rec t ((-1.0) *. h::temp)
      in
      inverse_rec v []
  
  (*Finding magnitude of vector*)    
  let length v = 
    if v=[] then 
      raise(DimensionError "Dimension should be greater than zero")
    else
      sqrt(dot_prod v v)

  (*Angle between two vectors*)    
  let angle v1 v2 =
    if v1=[]||v2=[] then 
      raise(DimensionError "Dimension should be greater than zero")
    else if dim v1 <> dim v2 then
      raise(DimensionError "Mismatched Dimensions")
    else 
      let dot = dot_prod v1 v2 in 
      let len1 = length v1 in 
      let len2 = length v2 in
      if len1 = 0.0 || len2 = 0.0 then
        raise (DimensionError "Length of vector cannot be zero")
      else if (dot /. (len1 *. len2))>1.0 then
        acos 1.0
      else if (dot /. (len1 *. len2))<(-1.0) then
        acos (-1.0)
      else
        acos (dot /. (len1 *. len2))
end;; 


open Vector;;

(* TEST CASES *)
(* Test Cases for create *)
let c1 = create 0 4.;; (* Expected answer is = exception *)
let c2 = create 1 5.67;; (* Expected answer is = [5.67] *)
let c3 = create 3 (-12.34);; (* Expected answer is = [-12.34; -12.34; -12.34] *)
let c4 = create 5 1.23;; (* Expected answer is = [1.23; 1.23; 1.23; 1.23; 1.23] *)
let c5 = create 10 4.56;; (* Expected answer is = [4.56; 4.56; 4.56; 4.56; 4.56; 4.56; 4.56; 4.56; 4.56; 4.56] *)

(* Test Cases for dim *)
let d1 = dim [1.0; 2.0; 3.0];; (* Expected answer is = 3 *)
let d2 = dim [4.0; 5.0];; (* Expected answer is = 2 *)
let d3 = dim [6.0];; (* Expected answer is = 1 *)
let d4 = dim [7.0; 8.0; 9.0; 10.0];; (* Expected answer is = 4 *)
let d5 = dim [];; (* Expected answer is = exception *)

(* Test Cases for is_zero *)
let z1 = is_zero [0.0; 0.0; 0.0];; (* Expected answer is = true *)
let z2 = is_zero [0.0; 0.0; 1.0];; (* Expected answer is = false *)
let z3 = is_zero [0.0; 0.0; 0.0; 0.0];; (* Expected answer is = true *)
let z4 = is_zero [0.0; 0.0; 0.0; 1.0];; (* Expected answer is = false *)
let z5 = is_zero [];; (* Expected answer is = exception *)

(* Test Cases for unit *)
let u1 = unit 5 3;; (* Expected answer is = [0.0; 0.0; 1.0; 0.0; 0.0] *)
let u2 = unit 4 2;; (* Expected answer is = [0.0; 1.0; 0.0; 0.0] *)
let u3 = unit 3 1;; (* Expected answer is = [1.0; 0.0; 0.0] *)
let u4 = unit 6 6;; (* Expected answer is = [0.0; 0.0; 0.0; 0.0; 0.0; 1.0] *)
let u5 = unit 2 3;; (* Expected answer is = exception *)

(* Test Cases for addv *)
let v1 = addv [1.0; 2.0; 3.0] [4.0; 5.0; 6.0];; (* Expected answer is = [5.0; 7.0; 9.0] *)
let v2 = addv [1.0; 2.0] [3.0; 4.0];; (* Expected answer is = [4.0; 6.0] *)
let v3 = addv [1.0] [2.0];; (* Expected answer is = [3.0] *)
let v4 = addv [1.0; 2.0; 3.0; 4.0] [5.0; 6.0; 7.0; 8.0];; (* Expected answer is = [6.0; 8.0; 10.0; 12.0] *)
let v5 = addv [] [];; (* Expected answer is = exception *)
let v6 = addv [1.0; 2.0] [];; (* Expected answer is = exception *)
let v7 = addv [1.1] [1.0; 2.0];; (* Expected answer is = exception *)

(* Test Cases for inverse *)
let i1 = inverse [1.0; 2.0; 3.0];; (* Expected answer is = [-1.0; -2.0; -3.0] *)
let i2 = inverse [4.0; 5.0];; (* Expected answer is = [-4.0; -5.0] *)
let i3 = inverse [6.0];; (* Expected answer is = [-6.0] *)
let i4 = inverse [7.0; 8.0; 9.0; 10.0];; (* Expected answer is = [-7.0; -8.0; -9.0; -10.0] *)
let i5 = inverse [];; (* Expected answer is = exception *)

(* Test Cases for dot_prod *)
let dp1 = dot_prod [1.0; 2.0; 3.0] [4.0; 5.0; 6.0];; (* Expected answer is = 32.0 *)
let dp2 = dot_prod [1.0; 2.0] [3.0; 4.0];; (* Expected answer is = 11.0 *)
let dp3 = dot_prod [1.0] [2.0];; (* Expected answer is = 2.0 *)
let dp4 = dot_prod [1.0; 2.0; 3.0; 4.0] [5.0; 6.0; 7.0; 8.0];; (* Expected answer is = 70.0 *)
let dp5 = dot_prod [] [];; (* Expected answer is = exception *)
let dp6 = dot_prod [1.0; 2.0] [];; (* Expected answer is = exception *)
let dp7 = dot_prod [1.1] [1.0; 2.0];; (* Expected answer is = exception *)

(* Test Cases for length *)
let l1 = length [1.0; 2.0; 3.0];; (* Expected answer is = 3.7416573867739413 *)
let l2 = length [4.0; 5.0];; (* Expected answer is = 6.4031242374328485 *)
let l3 = length [6.0];; (* Expected answer is = 6.0 *)
let l4 = length [7.0; 8.0; 9.0; 10.0];; (* Expected answer is = 15.0 *)
let l5 = length [];; (* Expected answer is = exception *)

(* Test Cases for scale *)
let s1 = scale 5.0 [1.0; 2.0; 3.0];; (* Expected answer is = [5.0; 10.0; 15.0] *)
let s2 = scale 3.0 [4.0; 5.0];; (* Expected answer is = [12.0; 15.0] *)
let s3 = scale 2.0 [6.0];; (* Expected answer is = [12.0] *)
let s4 = scale 4.0 [7.0; 8.0; 9.0; 10.0];; (* Expected answer is = [28.0; 32.0; 36.0; 40.0] *)
let s5 = scale 1.0 [];; (* Expected answer is = exception *)

(* Test Cases for angle *)
let a1 = angle [1.0; 2.0; 3.0] [4.0; 5.0; 6.0];; (* Expected answer is = 0.2257261285527342 *)
let a2 = angle [1.0; 2.0] [3.0; 4.0];; (* Expected answer is = 0.1798534997924783 *)
let a3 = angle [1.0] [2.0];; (* Expected answer is = 0.0 *)
let a4 = angle [1.0; 2.0; 3.0; 4.0] [5.0; 6.0; 7.0; 8.0];; (* Expected answer is = 0.0 *)
let a5 = angle [] [];; (* Expected answer is = exception *)
let a6 = angle [1.0; 2.0] [];; (* Expected answer is = exception *)
let a7 = angle [1.1] [1.0; 2.0];; (* Expected answer is = exception *)


(*PROOFS*)

(*DEEPAK SAGAR*)
(* int proofs + is addv function, +. is addition operator , might be some typo due to much usage 
same for other function too *)

(* Commutativity Proof => u+v=v+u 
Proof using : add v1 v2 function
Base Case :
for two single element vectors u = [a], v = [b] where a,b belongs to float numbers 
-> u+v=[a]+[b]=[a+b]
-> v+u=[b]+[a]=[b+a]=[a+b] by commmutativity of scalars
-> hence u+v = v+u
= Base case holds
Induction Hyothesis: Assume for vectors u and v of size n, we have commutativity holds i.e. u+v=v+u
Inductive Step: Let take vectors u1 and v1 of size n+1:
-> let u1 = h1::u and v1 = h2::v  
-> by using add function , u1+v1= (h1 +. h1)::(u+v)
-> by induction hypothesis , u+v=v+u
-> by simple commutatvity of two floating numbers (h1 +. h2) = (h2 +. h1)
-> therefore , u1+v1 = (h2 +. h1)::(v+u) = v1+u1
-> hence commutativity holds by induction
*)

(* Associativity Proof => u+(v+w)=(u+v)+w
Proof using : add v1 v2 function
Base Case:
for three single element vectors u = [a], v = [b] ,w =[c], where a,b,c belongs to floating numbers
    -> u+(v+w) = [a]+([b]+[c]) =[a+(b+c)]
    -> (u+v)+w = ([a]+[b])+[c] =[(a+b)+c] = [a+(b+c)] using associativy of scalars
    -> since u+(v+w)=(u+v)+w
    -> Base case holds 
  Induction Hyothesis: Assume for vectors u,v,w of size n, we have associativity holds i.e. u+(v+w)=(u+v)+w
  Inductive Step: Let take vectors u1,v1,w1 of size n+1:
    -> let u1 = h1::u, v1=h2::v, w1=h3::w
    -> by using add function,u1+(v1+w1)=(h1+.(h2+.h3))::(u+(v+w))
    -> also (u1+v1)+w1=((h1+.h2)+.h3)::((u+v)+w)
    -> By induction hypothesis we have u+(v+w)=(u+v)+w
    -> using associativity of simple float addition  h1+.(h2+.h3)=(h1+.h2)+.h3
    -> therefore u1+(v1+w1)=(h1+.(h2+.h3))::(u+(v+w))=(h1+.h2)+.h3::(u+v)+w=(u1+v1)+w1
    -> hence associativity holds by induction
*)

(* Identity of Addition Proof=> u+ZERO = u
here ZERO is ZERO vector (not scalar zero)
Proof using: add v1 v2 function
  Base Case:
    for any single element vector u = [a], where a belongs to floating number
    ->u+ZERO=[a]+[0]=[a+0]=[a]=u
    ->Base case holds
  Inductive Hyothesis: Assume for vector u of size n,we have identity of addition holds i.e. u+ZERO=u
  Inductive Step: Let take vector u1 of size n+1
    -> let u1 = h1::u
    -> by using add function, u1+ZERO = (h1 +. 0.0)::(u +. ZERO)
    -> By induction hypothesis we have u+ZERO = u
    -> using simple float identity of addition h1 +. 0.0 = h1
    -> therefore u1+ZERO = (h1 +. 0.0)::(u +. ZERO) = h1::u = u1
    -> Hence Identity of Addition Holds by Induction
*)

(* Identity Scalar Proof => 1.u = u
Here 1 is a scalar number
Proof using : scale c v function
  Base Case:
    for any single element vector u = [a], where a belongs to floating number
    ->1.u=1.[a]=[1*a]=[a]=u
    ->Base case holds
  Inductive hypothesis: Assume for vector u of size n, we have Identity Scalar Holds, i.e. 1.u =u
  Inductive Step: Let take vector u1 of size n+1
    -> let u1 = h::u
    -> using scale function, 1.u1 = (1. *. h)::(1.u)
    -> By induction hypothesis we have 1.u =u
    -> using simple float identity scalar (1. *. h)= h
    -> therefore 1.u1 = (1. *. h)::(1.u) = h::u=u1
    -> Hence Identity Scalar Proof Holds by induction
*)

(* Annihilator Scalar Proof=> 0.u = ZERO
Here 0 is scalar zero,  ZERO is a ZERO vector (not a scalar)
Proof using : scale c v function
  Base case:
    for any single element vector u = [a], where a belongs to floating number
    ->0.v=0.[a]=[0*.a]=[0.0]=ZERO
    ->Base case holds
  Inductive hypothesis: Assume for vector u of size n, we have Annihilator Scalar Holds, i.e. 0.u =ZERO
  Inductive Step: Let take vector u1 of size n+1
    -> let u1 = h::u
    -> using scale function, 0.u1 = (0. *. h)::(0.u)
    -> By induction hypothesis we have 0.u =ZERO
    -> using simple float Annihilator  scalar (0. *. h)= 0
    -> therefore 0.u1 = (0. *. h)::(0.u) = 0::ZERO=ZERO
    -> Hence Annihilator Scalar Proof Holds by induction
*)

(* Additive Inverse Proof => v + (- v) = ZERO
Here ZERO is a ZERO vector (not a scalar)
Proof using : add v1 v2 function
  Base Case:
    for any single element vector v = [a], where a belongs to floating number
    -> v+(-v)=[a]+[-a]=[a-a]=[0.0] = ZERO
    -> Base case holds
  Inductive hypothesis: Assume for vector v of size n, we have Additive inverse holds, i.e v + (- v) = ZERO
  Inductive Step: Let take vector v1 of size n+1
    -> let v1 = h::v
    -> using add function, v1+(-v)=(h+.(-h))::(v+(-v))
    -> by induction hypothesis we have v + (- v) = ZERO
    -> using simple float additive inverse we have (h+.(-h)) =0
    -> therefore v1+(-v)=(h+.(-h))::(v+(-v)) = 0::ZERO = ZERO
    -> Hence Additive Inverse holds by induction
*)

(* Scalar Product Combination Proof => b.(c.v) = (b.c).v
Proof using : scale c v function
  Base case:
    for any single element vector v = [a], where a belongs to floating number
    ->b.(c.v)=b.([c*.a])=[b*.(c*.a)] = [(b*. c)*.a] using scalars product combination
    ->(b.c).v=(b.c).[a]=[(b*. c)*.a] 
    ->Base case holds
  Inductive hypothesis: Assume for vector v of size n, we have Scalar Product Combination Holds, i.e. b.(c.v)=(b.c).v
  Inductive Step: Let take vector v1 of size n+1
    -> let v1 = h::v
    -> using scale function, b.(c.v1)=b.((c*.h)::(c.v))=(b*.(c*.h))::(b.(c.v))
    -> By induction hypothesis we have b.(c.v)=(b.c).v
    -> Using simple float Scalar Product Combination we have (b*.(c*.h))=(b*.c)*.h
    -> therefore b.(c.v1)=b.((c*.h)::(c.v))=(b*.(c*.h))::(b.(c.v))  = (b*.c)*.h::(b.c).v  = (b.c).v1
    -> Hence Scalar Product Combination Proof Holds by induction
*)

(* Scalar Sum-Product Distribution Proof => (b + c).v = b.v + c.v
Proof using : scale c v function
  Base case:
    for any single element vector v = [a], where a belongs to floating number
    ->(b+c).v=(b+c).[a]=[(b+c)*.a]=[(b*.a + c*.a)] using scalars distribution
    ->b.v+c.v=[b*.a]+[c*.a]=[(b*.a + c*.a)]
    ->Base case holds
  Inductive hypothesis: Assume for vector v of size n, we have Scalar Sum-Product Distribution Holds, i.e. (b + c).v = b.v + c.v
  Inductive Step: Let take vector v1 of size n+1
    -> let v1 = h::v
    -> using scale function, (b+c).v1 =((b+c)*.h)::((b+c).v)
    -> By induction hypothesis we have (b + c).v = b.v + c.v
    -> Using simple float Scalar Sum-Product Distribution we have ((b+c)*.h) = (b*.h)+(c*.h)
    -> therefore  (b+c).v1 =((b+c)*.h)::((b+c).v)  = ((b*.h)+(c*.h)) :: (b.v + c.v) = b.v1 + c.v1
    -> Hence Scalar Sum-Product Distribution Proof Holds by induction
*)

(* Scalar Distribution Over Vector Sums Proof => b.(u + v) = b.u + b.v
Proof using : add v1 v2 , scale c v function
  Base case:
    for two single element vectors u = [a], v = [a1] where a,a1 belongs to float numbers
    ->u+v=[a]+[a1] =[a+.a1]
    ->b.(u+v)=b.([a+.a1]) =[b*.(a+.a1)] = [b*.a +. b*.a1]
    ->b.u+b.v=b.([a])+b.([a1])=[b*.a]+[b*.a1] =[b*.a +. b*.a1]
    ->Base case holds
  Inductive hypothesis: Assume for vector u,v of size n, we have Scalar Distribution over Vector Sums Holds, i.e. b.(u+v)=b.u+b.v
  Inductive Step: Let take vectors u1,v1 of size n+1
    ->let u1 = h1::u , v1 = h2::v
    -> using add function, u1+v1= (h1+. h2)::(u+v)
    -> using scale function, b.(u1+v1)=b.((h1+.h2)::(u+v))=(b*.(h1+.h2))::b.(u+v)
    -> By induction hypothesis we have b.(u + v) = b.u + b.v
    -> b.u1 = (b*.h1)::b.u , b.v1=(b*.h2)::b.v)
    -> adding b.u1 and b.v1 => b.u1+b.v1=((b*.h1)+.(b*.h2))::(b.u+b.v)  = (b*.(h1+.h2))::b.(u+v) =  b.(u1+v1)
    -> comparing both equality holds
    -> Hence Scalar Distribution Over Vector Sums Proof Holds by induction
*)

(* MORE OTHER PROPERTIES *)

(* Dot Product Symmetry property(dot product)=> u.v=v.u
Proof using => dot_prod function
  Base Case:
    For any single element vectors u=[a],v=[b] , where a ,b are floating numbers 
    ->u.v=[a].[b]=a*.b
    ->v.u=[b].[a]=b*.a=a*.b
    ->Base case holds.
  Inductive hypothesis: Assume for vector u,v of size n, we have Dot Product Symmetry Holds, i.e. u.v=v.u
  Inductive Step: Let take vectors u1,v1 of size n+1
    ->let u1 = h1::u , v1 = h2::v
    ->using dot_prod function, u1.v1 = (h1*.h2) +. (u.v)
    -> By induction hypothesis we have (u.v)=(v.u)
    -> by float multiplication commutativity (h1*.h2)=(h2*.h1)
    -> therefore u1.v1=(h1*.h2) +. (u.v) =  (h2*.h1)+. (v.u) = (v1.u1)
    -> Hence Dot Product Summetry Holds using induction
*)

(* Triangle Inequality Proof (length)=> length(add(u,v))<=length(u)+length(v)
Proof using : add v1 v2 , length functions
  Base Case:
    for any single element vectors u=[a],v=[b], where a,b belongs to floating point numbers
    ->add([a],[b])=[a+b]
    ->length(add([a],[b])) = length([a+b])=sqrt(dot([a+b],[a+b])) = sqrt((a+b)*.(a+b)) = (a+.b)
    ->length(u)+.length(v) = length([a]) +. length([b]) =sqrt(dot([a],[a]))+.sqrt(dot([b],[b])) = (a+.b)
    ->base case holds
  Induction hypothesis: Assume for vector u,v of size , triangle inqeuality holds , i.e length(add(u,v))<=length(u)+length(v)
  Inductive Step: let take vectors u1,v2 of size n+1
    -> let u1 = h1::u , v1 = h2::v
    -> using add function, add(u1,v1)=(h1+h2)::add(u,v)
    -> using length function, length(add(u1,v1))=sqrt((h1+h2)^2+length(add(u,v))^2)
    -> using tringle inqeuality for scalars, |h1|+|h2| <= |h1+h2|
    -> using induction hypothesis , length(add(u,v))<=length(u)+length(v)
    -> combining all these, length(add(u1,v1))=sqrt((h1+h2)^2+length(add(u,v))^2) <= sqrt((|h1|+|h2|)^2 + (length(u)+length(v))^2)  <= length(u1)+length(v1)
    -> Hence Triangle Inequality holds by induction
*)

(* Angle of a vector with itself is 0 degree => angle(v,v)=0
Proof using: dot_prod,length,cos functions
  Concept: cos(angle(u,v))= (dot(u,v))/(length(u)*length(v))
  Base Case:
    for any empty vector v =[] => we raise error in our code
    for ant single element vector v = [a] , where a belong to floating point number
    ->dot(v,v)=a*.a
    ->length(v)=sqrt(a) 
    -> cos(angle(v,v)) = (a*.a)/(sqrt(a)*.(sqrt(a)) =1 it mean angle(v,v)=0 degree
    -> Base case holds
  Induction hypothesis: assume for a vector v of size n the property holds i.e. angle(v,v)=0 or cos(angle(v,v))=1
  Induction Step: let take vector v1 of size n+1
    ->let v1 = h::v
    ->using dot_prod function, dot(v1,v1) = (h*.h) +(dot(v,v))
    ->using length fucntion, length(v1)= sqrt(dot(v1,v1))
    ->finding cos(angle(v1,v1))=dot(v1,v1)/(length(v1)*(length(v2)))
    = ((h*.h) +(dot(v,v)))/( sqrt(dot(v1,v1)) * sqrt(dot(v1,v1)) )
    = ((h*.h) +(dot(v,v)))/dot(v1,v1)
    = ((h*.h) +(dot(v,v)))/((h*.h) +(dot(v,v)))
    = 1.0
    -> since cos(angle(v,v))=1.0 , i.e. angle(v,v)=0 degree
    -> hence proved Angle of a vector with itself is 0 degree 
*)


(* Angle of a vector with its inverse is 180 degree => angle(v,-v)=180
Proof using: dot_prod,length,cos functions
  Concept: cos(angle(u,v))= (dot(u,v))/(length(u)*length(v))
  Base Case:
    for any empty vector v =[] => we raise error in our code
    for ant single element vector v = [a] , where a belong to floating point number
    ->dot(v,-v)=a*.-a
    ->length(v)=sqrt(a) 
    -> cos(angle(v,-v)) = (a*.-a)/(sqrt(a)*.(sqrt(a)) =-1 it mean angle(v,-v)=180 degree
    -> Base case holds
  Induction hypothesis: assume for a vector v of size n the property holds i.e. angle(v,-v)=180 or cos(angle(v,-v))=-1
  Induction Step: let take vector v1 of size n+1
    ->let v1 = h::v
    ->using dot_prod function, dot(v1,-v1) = (h*.-h) +(dot(v,-v))
    ->using length fucntion, length(v1)= sqrt(dot(v1,v1))
    ->finding cos(angle(v1,-v1))=dot(v1,-v1)/(length(v1)*(length(v2)))
    = ((h*.-h) +(dot(v,-v)))/( sqrt(dot(v1,v1)) * sqrt(dot(v1,v1)) )
    = ((h*.-h) +(dot(v,-v)))/dot(v1,v1)
    = ((h*.-h) +(dot(v,-v)))/((h*.-h) +(dot(v,-v)))
    = -1.0
    -> since cos(angle(v,-v))=-1.0 , i.e. angle(v,-v)=180 degree
    -> hence proved Angle of a vector with its inverse is 180 degree

*)
(*Length Scaling : length(c⋅v)=∣c∣⋅length(v)
  Proof using : scale c v , length functions
  Base Case:
    for any empty vector v =[] => we raise error in our code
    for any single element vector v = [a] , where a belong to floating point number
    ->length(c.v)=length(c.[a])=length([c*.a])=sqrt((c*.a)*.(c*.a))=sqrt((c*.a)^2)=|c|*|a|=|c|*length(v)
    ->Base case holds
  Induction hypothesis: assume for a vector v of size n the property holds i.e. length(c.v)=|c|*length(v)
  Induction Step: let take vector v1 of size n+1
    -> let v1 = h::v
    -> using scale function, c.v1 = (c*.h)::(c.v)
    -> using length function, length(c.v1) = sqrt(dot(c.v1, c.v1))
    -> dot(c.v1, c.v1) = (c*.h)*.(c*.h) +. dot(c.v, c.v)
    -> length(c.v1) = sqrt((c*.h)*.(c*.h) +. dot(c.v, c.v))
    -> length(c.v1) = sqrt(c^2 *. (h*.h) +. c^2 *. dot(v, v))
    -> length(c.v1) = sqrt(c^2 *. (h*.h +. dot(v, v)))
    -> length(c.v1) = |c| *. sqrt(h*.h +. dot(v, v))
    -> length(c.v1) = |c| *. length(v1)
    -> Hence Length Scaling Proof Holds by induction
*)



