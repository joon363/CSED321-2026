type var = string

type tp =                         (* types *)
    Unit                                (* unit *)
  | Bool                                (* bool *)
  | Int                                 (* int *)
  | Fun of tp * tp                      (* tp -> tp *)
  | Prod of tp * tp                     (* tp * tp *)
  | Sum of tp * tp                      (* tp + tp *)

type exp =                        (* expressions *)
    Var of var                          (* variable *)
  | Lam of var * tp * exp               (* lambda abstraction *)
  | App of exp * exp                    (* application *)
  | Fix of var * tp * exp               (* fixed point construct *)
  | Pair of exp * exp                   (* pair (e1, e2) *)
  | Fst of exp                          (* projection fst *)
  | Snd of exp                          (* projection snd *)
  | Inl of exp * tp                     (* inleft *)
  | Inr of exp * tp                     (* inright *)
  | Case of exp * var * exp * var * exp (* case e of inl x1 -> e1 | inr x2 -> e2 *)
  | Eunit                               (* unit *)
  | True                                (* boolean true *)
  | False                               (* boolean false *)
  | Ifthenelse of exp * exp * exp       (* conditional construct *)
  | Num of int                          (* integer *)
  | Plus                                (* addition *)
  | Minus                               (* subtraction *)
  | Eq                                  (* equality *)

let rec tp2string t = match t with
    Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Fun (tp1, tp2) -> (tp2string tp1) ^ " -> " ^ (tp2string tp2)
  | Prod (tp1, tp2) -> (tp2string tp1) ^ " * " ^ (tp2string tp2)
  | Sum (tp1, tp2) -> (tp2string tp1) ^ " + " ^ (tp2string tp2)

let rec lam2string e = match e with
    Var x -> x
  | Eunit -> "()"
  | True -> "true"
  | False -> "false"
  | Num i -> string_of_int i
  | Plus -> "+"
  | Minus -> "-"
  | Eq -> "="
  | Lam (v, tp, e) -> "(fn " ^ v ^ " : " ^(tp2string tp) ^ " => " ^ (lam2string e) ^ ")"
  | App (e1, e2) -> "(" ^ (lam2string e1) ^ " " ^ (lam2string e2) ^ ")"
  | Fix (v, tp, e') -> "(fix " ^ v ^ " : " ^ (tp2string tp) ^ " => " ^ (lam2string e') ^")"
  | Pair (e1, e2) -> "(" ^ (lam2string e1) ^ ", " ^ (lam2string e2) ^ ")"
  | Fst e' -> "(fst " ^ (lam2string e') ^ ")"
  | Snd e' -> "(snd " ^ (lam2string e') ^ ")"
  | Inl (e', tp) -> "(inl (" ^ (tp2string tp) ^ ") " ^ (lam2string e') ^ ")"
  | Inr (e', tp) -> "(inr (" ^ (tp2string tp) ^ ") " ^ (lam2string e') ^ ")"
  | Case (e', v1, e1, v2, e2) -> "(case " ^ (lam2string e') ^ " of inl " ^ v1 ^ " => " ^ (lam2string e1) ^ " | " ^ " inr " ^ v2 ^ " => " ^ (lam2string e2) ^ ")"
  | Ifthenelse (e1, e2, e3) -> "(if " ^ (lam2string e1) ^ " then " ^ (lam2string e2) ^ " else " ^ (lam2string e3) ^ ")"
