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
let rec stepv e = raise NotImplemented

let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e =
    match (stepOpt stepf e) with
      None -> Seq.empty
    | Some e' -> fun () -> Seq.Cons (e', steps e')
  in
  fun () -> Seq.Cons (e, steps e)
