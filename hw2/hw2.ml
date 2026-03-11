exception NotImplemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

(** Recursive functions **)

let rec lrevrev l = 
  (* Using functions from HW1 *)
  let rec lmap f l = 
    match l with
    (* Stop Condition *)
    | [] -> []
    | [x] -> [(f x)]
    (* Recursion: apply head and iterate. *)
    | h::t -> (f h) :: (lmap f t) in
  let rec lrev l = 
    match l with
    (* Recursion: apply head and iterate. *)
    | h::t -> (lrev t) @ [h]
    (*Stop Condition (empty list)*)
    | _ -> []
  (* Reverse the elements, and reverse it again. *)
  in lrev (lmap lrev l)

let rec lfoldr f e l = 
  (* for list a b c, returns f(a,(f(b,(f(c,e))))) 
  Note that type of f is (a*b->b) *)
  match l with
  | [] -> e
  | h::t -> f (h, (lfoldr f e t))

(** Tail-recursive functions  **)

let fact n = 
  (* f 3 1 => f 2 (3*1) => f 1 (2*3*1) => f 0 (1*2*3*1) => (1*2*3*1) *)
  let rec fact_aux n acc = 
    if n=0 then acc
    else fact_aux (n-1) (n*acc)
  in fact_aux n 1

let fib n = 
  (* Challange is that the fact that fib n = fib (n-1) + fib(n-2)
  is NOT tail-recursive.
  To implement it in tail-recursive
  f(a,b,n) = f(b,(a+b),n-1) until n reaches 0 
  
  ex. fib 4 (1 1 2 3 5)
  f(0,1,4) = f(1,1,3) = f(1,2,2) = f(2,3,1) = f(3,5,0) = 5 *)
  let rec fib_aux a b count = 
    if count=0 then b
    else fib_aux b (a+b) (count-1)
  in fib_aux 0 1 n

let asum l = 
  (* We need binary flag for operation and sum for tail rec.
  flag=0: +, flag=1: -
  f [3 2 7 3 ] 0 0 
  = f [2 7 3] 1 0+3 
  = f [7 3] 0 0+3-2
  = f [3] 1 0+3-2+7
  = f [] 0 0+3-2+7-3
  *)
  let rec aux l flag res = 
    (* Helper functions *)
    let plusminus_f flag x = 
      if flag=0 then x
      else (-1)*x
    in let newflag_f flag = 
      if flag=0 then 1
      else 0
    in match l with
    (* Stop Condition *)
    | [] -> res
    | h::t -> 
      let newflag = newflag_f flag in
      let plusminus = plusminus_f flag h in
      aux t newflag res+plusminus
  (* Start recursion with flag=0, sum=0 *)
  in aux l 0 0

let ltabulate n f = 
  (* Note that this might similar to the lmap in hw1
  but it should make a list and also tail-rec. 
  aux 3 [] = aux 2 4::[] = aux 1 1::4::[] = aux 0 0::1::4::[] = [0,1,4]
  *)
  let rec aux n acc =
    if n=0 then acc
    else aux (n-1) ((f (n-1))::acc)
  in aux n []

let lfilter p l = 
  (* Let's make a acc list and append the header at the tail only when p h = true 
  Note) since '@' operator is heavy, we might start with reversed list.
  however, efficiency is not the key in this assignment. so, I'll just use '@' operator.
  *)
  let rec aux l acc = 
    match l with
    | [] -> acc
    | h::t -> 
      let add_elem = if p h then [h] else []
      in aux t (acc@add_elem)
  in aux l []

let union s t = 
  (* Naive idea: just check the whole list every time
  or, make s@t and check from first element, and delete any
  i.e. 
  1 2 3 2 4 6 []
  -> 2 3 2 4 6 [1]
  -> 3 4 6 [1 2]
  -> 4 6 [1 2 3]
  -> 6 [1 2 3 4]
  ->[1 2 3 4 6]
  *)
  let rec acc l res = 
    (* We can use filter here. for 2::[3,2,4,6], lfilter (fun x->x!=2) [3,2,4,6] = [3,4,6] *)
    match l with
    | [] -> res
    | h::t -> acc (lfilter (fun x -> x!=h) t) (h::res)
  in acc (s@t) []

let inorder t =
   (* Note: just rec version: (inorder l) @ [v] @ (inorder r)
   However, it is not tail-rec. 
   Idea: I have absolutly no idea how to implement as the pdf says.. (tree, list)
   So I will use "tree list" and "list" instead of "tree" "list"*)
  let rec aux subtrees post = 
    match subtrees with 
    (* Stop Condition *)
    | [] -> post
    | h::t -> 
      match h with
      | Leaf v -> aux t (post @ [v])
      (* Inorder Traverse *)
      | Node (l,v,r) -> aux ([l;(Leaf v);r]@t) (post)
    in aux [t] []


let postorder t = 
  let rec aux subtrees post = 
    match subtrees with 
    (* Stop Condition *)
    | [] -> post
    | h::t -> 
      match h with
      | Leaf v -> aux t (post @ [v])
      (* Postorder Traverse *)
      | Node (l,v,r) -> aux ([l;r;(Leaf v)]@t) (post)
    in aux [t] []

let preorder t = 
  let rec aux subtrees post = 
    match subtrees with 
    (* Stop Condition *)
    | [] -> post
    | h::t -> 
      match h with
      | Leaf v -> aux t (post @ [v])
      (* Preorder Traverse *)
      | Node (l,v,r) -> aux ([(Leaf v);l;r]@t) (post)
    in aux [t] []

(** Sorting functions **)

let rec quicksort l = 
  match l with
  | [] -> []
  | h::t ->
    (* pivot: h *)
    (* use lfilter to make two lists *)
    let left = lfilter (fun x -> x<=h) t in
    let right = lfilter (fun x -> x>h) t in
    (quicksort left) @ [h] @ (quicksort right)
     

let rec mergesort l = 
  (* merge helper func. compare first elements and select smaller one and go on. *)
  let rec merge l1 l2 = 
    match (l1,l2) with
    | [], [] -> []
    | [x], [] -> [x]
    | [], [x] -> [x]
    | h1::t1, h2::t2 ->
      if h1<h2 then h1::(merge t1 (h2::t2))
      else h2::(merge (h1::t1) t2)
  (* divide helper func. since we don't know the length of the list, divide by even and odd position. *)
  in let rec divide li left right flag =
    match li with
    | [] -> (left, right) 
    | h::t ->
      if flag=0 then divide t (h::left) right 1
      else divide t left (h::right) 0
  in
  (* mergesort entry *)
  match l with
  | []-> []
  | [x] -> [x]
  | h::t -> 
    (* Divide*)
    let (left, right) = divide l [] [] 0 in
    (* Recurse and Conquer *)
    merge (mergesort left) (mergesort right)

(** Structures **)

module type HEAP =
  sig
    exception InvalidLocation
    type loc
    type 'a heap
    val empty : unit -> 'a heap
    val allocate : 'a heap -> 'a -> 'a heap * loc
    val dereference : 'a heap -> loc -> 'a
    val update : 'a heap -> loc -> 'a -> 'a heap
  end

module type DICT =
  sig
    type key
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict
  end

module Heap : HEAP =
  struct
    exception InvalidLocation

    (* Lets assume 2^32 Byte (4GB) address(location) space *)
    type loc = int 
    (* Heap Memory has: location and value
    note that we assume 'a is 1 byte; such as char.
    else, we need to implement REAL dynamic allocation. but that is out of the scope of this assignment. *)
    (* Also, It seems that using map or bucket will be more efficient.. but efficiency is also not in our consideration. *)
    type 'a heap = (loc * 'a) list

    let empty _ = []
    let allocate h v = 
      (* Append to the end of the list *)
      let new_loc = List.length h in
      ((new_loc, v)::h, new_loc)

    let dereference h l = 
      try
        (* Use assoc function in association lists. see https://ocaml.org/manual/5.4/api/List.html 
        Also, there is no garbage collection. *)
        List.assoc l h
      with 
      | Not_found -> raise InvalidLocation
      | e -> raise e

    let update h l v = 
      (* Remove using remove_assoc, and append at the top.
      It would works since the list does not has to be in order of the location.
      Since remove_assoc does not raises exception when there is no element l, I used mem_assoc to determine whether l is valid or not.
      *)
      let doesExists = List.mem_assoc l h in
      if doesExists then (l, v) ::(List.remove_assoc l h)
      else raise InvalidLocation
  end

module DictList : DICT with type key = string =
  struct
    type key = string
    type 'a dict = (key * 'a) list

    let empty _ = raise NotImplemented
    let lookup _ _ = raise NotImplemented
    let delete _ _ = raise NotImplemented
    let insert _ _ = raise NotImplemented
  end

module DictFun : DICT with type key = string =
  struct
    type key = string
    type 'a dict = key -> 'a option

    let empty _ = raise NotImplemented
    let lookup _ _ = raise NotImplemented
    let delete _ _ = raise NotImplemented
    let insert _ _ = raise NotImplemented
  end
