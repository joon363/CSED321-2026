(*
 * Call-by-value reduction
 *)

exception NotImplemented
exception Stuck

let freshVarCounter = ref 0

(*
 * getFreshVariable : string -> string
 * you may use this function if you want to generate a fresh variable from s.
 *)
let getFreshVariable s =
  let _ = freshVarCounter := !freshVarCounter + 1
  in
  s ^ "__" ^ (string_of_int (!freshVarCounter))

(*
 * implement a single step with reduction using the call-by-value strategy.
 *)
let rec stepv exp = 
  match exp with
  | Var (v) -> raise Stuck 
  | Lam (x, e) -> raise Stuck 
  | App (e1, e2) ->
    match e1, e2 with
    (* Here we implement rule App, call [v/x]e *)
    
    | Lam (x, e), Var (_) | Lam (x, e), Lam (_, _)->
        (* print_string ("Rule App with e1: "^(exp2string e1)^" e2: "^(exp2string e2)^"\n"); *)
        substitute e2 x e

    (* Here we implement rule Arg, reduce e2. *)
    | Lam (x, e), _ ->
        let e2' = stepv e2 in
        (* print_string ("Rule arg with e1: "^(exp2string e1)^" e2: "^(exp2string e2)^"\n"); *)
        App (Lam (x, e), e2')

    (* Here we implement Rule Lam *)
    | App(_, _), _ -> 
      let e1' = stepv e1 in
        (* print_string ("Rule Lam with e1: "^(exp2string e1)^" e2: "^(exp2string e2)^"\n"); *)
        App (e1', e2)

    (* The only case left is that the first argument of application is NOT an lambda expression. 
    I want to raise Stronger exception but let's just use Stuck here. *)
    | _,_ -> raise Stuck

      


let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e =
    match (stepOpt stepf e) with
      None -> Seq.empty
    | Some e' -> fun () -> Seq.Cons (e', steps e')
  in
  fun () -> Seq.Cons (e, steps e)
