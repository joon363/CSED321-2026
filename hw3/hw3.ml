open Common

exception NotImplemented

exception IllegalFormat

module Integer : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = 1

  let (++) x y = x + y
  let ( ** ) x y = x * y
  let (==) x y = x = y
end

(* Problem 2-1 *)
(* Scalar *)

module Boolean : SCALAR with type t = bool
=
struct
  type t = bool

  exception ScalarIllegal

  let zero = false
  let one = true

  let (++) x y = x||y
  let ( ** ) x y = x&&y
  let (==) x y = x=y
end

(* Problem 2-2 *)
(* Vector *)

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t
=
struct
  type elem = Scal.t
  type t = elem list (* Simple Implementation.*)

  exception VectorIllegal

  (* vector creation.
  - create takes a list of scalar elements, and returns a corresponding vector:
    create [x0; ...; xi; ...; x(n-1)] = a vector /x0; ...; xi; ...; x(n-1)/.
  - VectorIllegal is raised if the list is empty. *)
  let create l = 
    match l with
    | [] -> raise VectorIllegal
    | _ -> l

  (* to_list.
  - to_list takes a vector, and converts it to a list of scalar elements:
    to_list /x0; ...; xi; ...; x(n-1)/ = [x0; ...; xi; ...; x(n-1)]. *)
  let to_list v = 
    v (* Because we are using list! *)

  (* dimension.
  - dim v returns the dimension of v: the number of scalar elements in v. *)
  let dim v = 
    List.length(v)

  (* extraction.
  - nth v n returns the n-th scalar element of v, where indexes begin at 0.
  - VectorIllegal is raised if n is out of range. *)
  let nth v n = 
    (* nth function at https://ocaml.org/manual/5.4/api/List.html
    Return the n-th element of the given list. The first element (head of the list) is at position 0.
    Raises
      Failure if the list is too short.
      Invalid_argument if n is negative.
    
        We just need to raise VectorIllegal with Failure or Invalid_argument exception.*)
    try List.nth v n
    with _ -> raise VectorIllegal

  (* vector addition.
  - ++ is associative: x ++ (y ++ z) = (x ++ y) ++ z.
  - ++ is commutative: x ++ y = y ++ x.
  - VectorIllegal is raised if x and y have different dimensions. *)
  let (++) v1 v2 = 
    (* Let's apply SCALAR addition for each element of two lists.
    I used map2 and compare_lengths function in module List.
    compare_lengths is 0 if equal size, else 1 or -1 *)
    if (List.compare_lengths v1 v2)=0
      then List.map2 Scal.(++) v1 v2
    else raise VectorIllegal

  (* equality test.
  - (x == y) = true iff x = y.
  - VectorIllegal is raised if x and y have different dimensions. *)
  let (==) v1 v2 = 
    (* I used List.equal function but it cannot detect different lengths.
    So I used compare_lengths first to assure it.*)
    if (List.compare_lengths v1 v2)=0
        then List.equal Scal.(==) v1 v2
    else raise VectorIllegal

  (* inner product.
  - inner x y returns the inner product of x and y by using ++ and ** operations for type elem: inner /x0; ...; xn/ /y0; ...; yn/ = (x0 ** y0) ++ ... ++ (xn ** yn).
  - VectorIllegal is raised if x and y have different dimensions. *)
  let innerp v1 v2 = 
    if (List.compare_lengths v1 v2)=0 then
      let l = List.map2 Scal.( ** ) v1 v2 in (* [x0y0; x1y1; ...; xnyn] *)
      List.fold_left Scal.(++) Scal.zero l (* 0+x0y0+...xnyn*)
    else raise VectorIllegal
end

(* Problem 2-3 *)
(* Matrix *)

module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t
=
struct
  type elem = Scal.t
  type t = unit

  exception MatrixIllegal

  let create _ = raise NotImplemented
  let identity _ = raise NotImplemented
  let to_list _ = raise NotImplemented
  let dim _ = raise NotImplemented
  let transpose _ = raise NotImplemented
  let get _ _ _ = raise NotImplemented

  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* Problem 3-1 *)
(* Closure *)

(* module ClosureFn (Mat : MATRIX) :
sig
  val closure : Mat.t -> Mat.t
end
=
struct
  let closure _ = raise NotImplemented
end

(* Problem 3-2 *)
(* Reachability Problem *)

module BoolMat = MatrixFn (Boolean)
module BoolMatClosure = ClosureFn (BoolMat)

let reach _ = raise NotImplemented

let al =
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  false; false];
   [false; true;  true;  false; true;  false];
   [false; true;  false; true;  true;  true];
   [false; false; true;  true;  true;  false];
   [false; false; false; true;  false; true]]

let solution_al' =
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true]]

(* Problem 3-3 *)
(* Shortest Distance Problem *)

module Distance : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)

  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let distance _ = raise NotImplemented

let dl =
  [[  0;  -1;  -1;  -1;  -1;  -1 ];
   [ -1; 0  ; 35 ; 200; -1 ; -1  ];
   [ -1; 50 ; 0  ; -1 ; 150; -1  ];
   [ -1; 75;  -1 ; 0  ; 100; 25  ];
   [ -1; -1 ; 50 ; 65 ; 0  ; -1  ];
   [ -1; -1 ; -1 ; -1 ; -1 ; 0   ]]

let solution_dl' =
  [[0;  -1;  -1;  -1;  -1;  -1  ];
   [-1; 0;   35;  200; 185; 225 ];
   [-1; 50;  0;   215; 150; 240 ];
   [-1; 75;  110; 0;   100; 25  ];
   [-1; 100; 50;  65;  0;   90  ];
   [-1; -1;  -1;  -1;  -1;  0   ]]

(* Problem 3-4 *)
(* Maximum Weight Problem *)

module Weight : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)

  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let weight _ = raise NotImplemented

let ml =
  [[-1; 0  ; 0  ; 0  ; 0  ; 0   ];
   [0 ; -1 ; 10 ; 100; 0  ; 0   ];
   [0 ; 50 ; -1 ; 0  ; 150; 0   ];
   [0 ; 75 ; 0  ; -1 ; 125; 40 ];
   [0 ; 0  ; 25 ; -1 ; -1 ; 0   ];
   [0 ; 0  ; 0  ; 0  ; 0  ; -1  ]]

let solution_ml' =
  [[-1; 0;  0;   0;   0;   0  ];
   [0;  -1; 25;  100; 100; 40 ];
   [0;  75; -1;  150; 150; 40 ];
   [0;  75; 25;  -1;  125; 40 ];
   [0;  75; 25;  -1;  -1;  40 ];
   [0;  0;  0;   0;   0;   -1 ]]

let _ =
  try
  if reach al = solution_al' && distance dl = solution_dl' && weight ml = solution_ml' then
    print_endline "\nYour program seems fine (but no guarantee)!"
  else
    print_endline "\nYour program might have bugs!"
  with _ -> print_endline "\nYour program is not complete yet!" *)
