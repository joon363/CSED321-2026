exception Not_implemented
exception Invariant

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let rec fac n = 
    if n<0 then raise Invariant  
    (*Stop condition: 1!=1*) 
    else if n=1 then 1
    else n * fac (n-1)
        
let rec power x n = 
    if n<0 then raise Invariant
    (*Stop condition: x^0=1*) 
    else if n=0 then 1
    else x * power x (n-1)

let rec gcd m n = 
    if n<0 || m<0 || m+n<1 then raise Invariant
    (*Euclid's algorithm
    for m>n, if m=nq+r then gcd m n = gcd n r*)
    else let rec inner a b = 
        (*Stop condition: reached r or r=0*)
        if a <= b then a 
        else inner (a-b) b in
    if m>n then inner m n else inner n m
        
let rec combi n k =
    if n<1 || k<0 || n<k then raise Invariant
    (*Stop Condition: nCn = 1, nCk = 1*)
    else if n=k || k=0 then 1 
    (*Pascal's triangle; nCk = n-1Ck-1 + n-1Ck*)
    else combi (n-1) (k-1) + combi (n-1) k 


let rec sum_tree t = 
    match t with
    (*Stop Condition: leaf *)
    | Leaf v -> v
    | Node (l,v,r) -> (sum_tree l) + v + (sum_tree r)
    | _ -> raise Invariant

let rec depth t = 
    (*Add accumulator*)
    let rec acc t d = 
    match t with
    (*Stop Condition: leaf *)
    | Leaf _ -> d
    (*Node: depth+=1, compare left and right*)
    | Node (l,_,r) ->
        let ldepth = acc l (d+1) in
        let rdepth = acc r (d+1) in
        if ldepth>rdepth then ldepth else rdepth
    | _ -> raise Invariant
    (*Start from root*)
    in acc t 0

let rec bin_search t x = 
    match t with
    (*return if value is x*)
    | Leaf v -> x=v
    (*find from node*)
    | Node (l,v,r) ->
        if x=v then true (*found!*)
        else if x<v then bin_search l x
        else bin_search r x
    | _ -> raise Invariant
    
let rec inorder t = 
    match t with
    | Leaf v -> [v]
    (*Concat by l-v-r*)
    | Node (l,v,r) -> (inorder l) @ [v] @ (inorder r)
    | _ -> raise Invariant


let rec max l = 
    match l with
    (*Empty List*)
    | [] -> 0
    (*Stop Condition*)
    | [x] -> x
    (*Max between head and else*)
    | h::t -> 
        let submax = max t in
        if h>submax then h else submax

let rec list_add l1 l2 = 
    (*Pattern match both list at once*)
    match (l1, l2) with
    | (h1::t1, h2::t2) -> (h1+h2)::(list_add t1 t2)
    | (h::t, [x]) | ([x], h::t) -> (h+x)::t
    | (l, []) | ([], l) -> l

let rec insert m l = 
    match l with
    (*Empty List*)
    | [] -> [m]
    (*Stop Condition*)
    | [x] -> if m<x then [m;x] else [x;m]
    (*Max between head and else*)
    | h::t -> if m<h then m::(h::t) else h::(insert m t)

let rec insort l = 
    (*for h::t, insert h::t will move h into proper place in t.
    that is, we can implement sort by insert h (insort t) *)
    match l with
    (*Empty List*)
    | [] -> []
    (*Single List*)
    | [x] -> [x]
    (*Max between head and else*)
    | h::t -> insert h (insort t)

let rec compose _ _ = raise Not_implemented
let rec curry _ _ _ = raise Not_implemented
let rec uncurry _ _ = raise Not_implemented
let rec multifun _ _ = raise Not_implemented

let rec ltake _ _ = raise Not_implemented
let rec lexists _ _ = raise Not_implemented
let rec lmap _ _ = raise Not_implemented
let rec lrev _ = raise Not_implemented
let rec lflat _ = raise Not_implemented
let rec lzip _ _ = raise Not_implemented
let rec split _ = raise Not_implemented
let rec cartprod _ _ = raise Not_implemented
let rec powerset _ = raise Not_implemented
