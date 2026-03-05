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


let rec sum_tree _ = raise Not_implemented
let rec depth _ = raise Not_implemented
let rec bin_search _ _ = raise Not_implemented
let rec inorder _ = raise Not_implemented

let rec max _ = raise Not_implemented
let rec list_add _ _ = raise Not_implemented
let rec insert _ _ = raise Not_implemented
let rec insort _ = raise Not_implemented

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
