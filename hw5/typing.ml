open Tml

exception TypeError

(*****************************************************
 * replace unit by your own type for typing contexts *
 *****************************************************)
(* Let's use DictFun from HW2 *)
type context = Tml.var -> Tml.tp

(* Insert: context -> (Tml.var * Tml.tp) -> context *)
let insert cxt (k, v) = 
  fun x -> (if x=k then v else cxt x)

(* Lookup: context -> Tml.var -> Tml.tp *)
let lookup cxt v = cxt v

(*
 * For each function you introduce,
 * write its type, specification, and invariant.
 *)

let createEmptyContext () = 
  (* No type at all; if you try to do find a type for key k, raise error. *)
  fun k -> raise TypeError 

(* val typing : context -> Tml.exp -> Tml.tp *)
let rec typing cxt e = 
  match e with
  (* Rule Var *)
  | Var x -> cxt x

  (* Rule ->I *)
  | Lam (x, a, exp) -> 
    let newCxt = insert cxt (x, a) in (* Γ, x:A *)
    let b = typing newCxt exp in      (* ⊢ e: B *)
    Tml.Fun (a, b)                    (* e: A->B *)

  (* Rule ->E *)
  | App (e1, e2) ->
    let e1type = typing cxt e1 in (
    match e1type with
    | Tml.Fun (a, b) ->
      let e2type = typing cxt e2 in 
      (* OK! Γ ⊢ e: A->B, Γ ⊢ e':A *)
      if e2type = a then b
      else raise TypeError
    | _ -> raise TypeError )

  (* Rule Fix *)
  | Fix (x, a, exp) ->
    let newCxt = insert cxt (x, a) in (* Γ, x:A *)
    let eType = typing newCxt exp in  (* ⊢ e:A *)
    if eType = a then a               (* Γ ⊢ fix x:A.e : A *)
    else raise TypeError

  (* Rule xI *)
  | Pair (e1, e2) ->
    (* If any error raised in e1 or e2, it would propagate. so I'll not do additional error handling. *)
    let a1 = typing cxt e1 in (* Γ ⊢ e1:A1 *)
    let a2 = typing cxt e2 in (* Γ ⊢ e2:A2 *)
    Tml.Prod (a1, a2)         (* Γ ⊢ e2:A2 *)

  (* Rule xE1 *)
  | Fst exp ->
    let eType = typing cxt exp in ( (* Γ ⊢ e:A1xA2 *)
    match eType with
    | Prod (a1, a2) -> a1           (* Γ ⊢ fst e:A1 *)
    | _ -> raise TypeError )
  (* Rule xE2 *)
  | Snd exp -> 
    let eType = typing cxt exp in ( (* Γ ⊢ e:A1xA2 *)
    match eType with
    | Prod (a1, a2) -> a2         (* Γ ⊢ snd e:A2 *)
    | _ -> raise TypeError )

  (* Rule +IL *)
  | Inl (exp, a2) ->
    let a1 = typing cxt exp in   (* Γ ⊢ e:A1 *)
    Sum (a1, a2)                 (* Γ ⊢ InlA2e:A1+A2 *)
  (* Rule +IR *)
  | Inr (exp, a1) ->
    let a2 = typing cxt exp in   (* Γ ⊢ e:A2 *)
    Sum (a1, a2)                 (* Γ ⊢ InrA1e:A1+A2 *)
  
  (* Rule +E *)
  | Case (exp, x1, e1, x2, e2) ->
    let eType = typing cxt exp in (
    match eType with
    | Sum (a1, a2) ->                         (* Γ ⊢ e:A1+A2 *)
      let newCxtx1 = insert cxt (x1, a1) in
      let newCxtx2 = insert cxt (x2, a2) in
      let c = typing newCxtx1 e1 in          (* Γ, x1:A1 ⊢ e1:C *)
      let c2 = typing newCxtx2 e2 in         (* Γ, x2:A2 ⊢ e2:C *)
      if c = c2 then c                        (* Γ ⊢ case e of inl x1, e1 | inr x2, e2: C*)
      else raise TypeError
    | _ -> raise TypeError )

  (* Rule Unit *)
  | Eunit -> Tml.Unit

  (* Rule True *)
  | True -> Tml.Bool
  (* Rule False *)
  | False -> Tml.Bool

  (* Rule If *)
  | Ifthenelse (exp, e1, e2) ->
    let eType = typing cxt exp in
    let a = typing cxt e1 in
    let a2 = typing cxt e2 in
    if eType = Tml.Bool && a = a2 then  (* Γ ⊢ e:bool, Γ ⊢ e1:A, Γ ⊢ e2:A *)
      a                                 (* Γ ⊢ if e then e1 else e2: A *)
    else raise TypeError

  (* Rule Int *)
  | Num n -> Tml.Int (* Γ ⊢ n: int *)

  (* Rule Plus *)
  | Plus -> Fun ((Prod (Int, Int)), Int) (* Γ ⊢ plus: int x int -> int *)
  (* Rule Minus *)
  | Minus -> Fun ((Prod (Int, Int)), Int) (* Γ ⊢ minus: int x int -> int *)
  (* Rule Eq *)
  | Eq -> Fun ((Prod (Int, Int)), Bool) (* Γ ⊢ eq: int x int -> bool *)

let typeOf e = typing (createEmptyContext ()) e
let typeOpt e = try Some (typeOf e)
                with TypeError -> None
