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
        (*always a>b*)
        if b=0 then a
        else if (a-b) <= b then inner b (a-b)
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
    
let rec inorder t = 
    match t with
    | Leaf v -> [v]
    (*Concat by l-v-r*)
    | Node (l,v,r) -> (inorder l) @ [v] @ (inorder r)


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

(*It does not have to be a recursive function*)
let compose f g = 
    let func x = g(f(x)) in func

(*It does not have to be a recursive function*)
let curry f = 
    let func x y = f (x,y) in func

(*It does not have to be a recursive function*)
let uncurry f = 
    let func (x,y) = f x y in func
let rec multifun f n = 
    if n<0 then raise Invariant
    (*Stop Condition*)
    else if n=1 then f
    (*Use compose above and recurse.*)
    else compose f (multifun f (n-1))

let rec ltake l n = 
    if n<0 then raise Invariant
    else let rec inner input buffer n = 
        match (input,n) with
        (*Stop Condition*)
        | _, 0 -> buffer
        | [], _ -> buffer
        | [x], _ -> buffer @ [x]
        (*Recursion: add head to the tail of the buffer list.*)
        | h::t, v -> inner t (buffer @ [h]) (v-1)
    in inner l [] n

let rec lexists f l = 
    match l with
    (*Stop Condition*)
    | [] -> false
    | [x] -> f x
    (*Recursion: logical or*)
    | h::t -> (f h) || lexists f t

let rec lmap f l = 
    match l with
    (*Stop Condition*)
    | [] -> []
    | [x] -> [(f x)]
    (*Recursion: apply head and iterate.*)
    | h::t -> (f h) :: (lmap f t)
    
let rec lrev l = 
    match l with
    (*Recursion: apply head and iterate.*)
    | h::t -> (lrev t) @ [h]
    (*Stop Condition (empty list)*)
    | _ -> []

let rec lflat l = 
    match l with
    (*Recursion: head is a' list*)
    | h::t -> h @ (lflat t)
    (*Stop Condition (empty list)*)
    | _ -> []

let rec lzip l1 l2 = 
    match (l1,l2) with
    (*Recursion: zip one-by-one*)
    | (h1::t1, h2::t2) -> (h1,h2) :: lzip t1 t2
    (*Stop Condition: empty or different length list; ignore surplus elements*)
    | (l, []) -> []
    | ([], l) -> []

let rec split l = 
    (*Recursion: make two buffers o(odd) and e(even)
    and use bool isOdd to indicate the position.
    output of inner o e : a' list, a' list *)
    let rec inner input o e isOdd= 
        match input with
        | h::t -> 
            if isOdd=true then inner t (o @ [h]) e false
            else inner t o (e @ [h]) true
        | [] -> (o,e)
    in inner l [] [] true
            

let rec cartprod s t = 
    (*Helper function: cartprod in one list*)
    (* inner x [y1 y2 y3] = [(x,y1), (x,y2), (x,y3)]*)
    let rec inner v l = 
        match l with
        | h::t -> (v,h)::(inner v t)
        | _ -> []
    (*Iterate elements of s with inner*)
    in match s with
    | h::tail -> (inner h t) @ (cartprod tail t) 
    | _ -> []

let rec powerset s = 
    (*Idea: for power [a,b,c], power set would be a::[power b,c] @ [power b,c]
    helper function fun v1, l -> v1::l
    ex) [a,b] -> 
    1. lmap (fun l->a::l) (power [b]) = lmap (fun l->a::l) ([[],[b]])
    = [[a], [a,b]]
    2. power [b] = [[],[b]]
    therefore [[],[b],[a],[a,b]]
    Also, to reduce computation, do memoization by let res = power b,c
    *)
    match s with
    | [] -> [[]]
    | [x] -> [[];[x]]
    | h::t -> 
        let res1 = powerset t in
        let res2 = lmap (fun l -> h::l) res1 in
        res1 @ res2

