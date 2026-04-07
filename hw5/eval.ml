exception NotImplemented
exception Stuck

let rec step e = raise Stuck

let stepOpt e = try Some (step e) with Stuck -> None

let rec multiStep e = try multiStep (step e) with Stuck -> e

let stepStream e =
  let rec steps e =
    match (stepOpt e) with
      None -> Seq.empty
    | Some e' -> fun () -> Seq.Cons (e', steps e')
  in
  fun () -> Seq.Cons (e, steps e)
