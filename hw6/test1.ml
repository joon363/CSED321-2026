open Tml
open Eval

let parse_str s = 
  let s = s ^ ";" in 
  Parser.parse Lexer.token (Lexing.from_string s)

type test_case = string * string * Tml.exp

let test_cases = [
  (* Variable / Lambda / Application *)
  ("Basic Lam",
   "fn x:int => x",
   Lam (Ind 0));

  ("Nested Lam",
   "fn x:int => fn y:int => x y",
   Lam (Lam (App (Ind 1, Ind 0))));

  ("Application Chain",
   "f x y",
   App (App (Ind 2, Ind 1), Ind 0));

  (* Free variables *)
  ("Free variables 1",
   "a b c a",
   App (App (App (Ind 0, Ind 2), Ind 1), Ind 0));

  ("Free variables 2",
   "fn x:int => a x b c",
   Lam (App (App (App (Ind 3, Ind 0), Ind 2), Ind 1)));

  (* Pair / fst / snd *)
  ("Pair",
   "(1, true)",
   Pair (Num 1, True));

  ("Fst",
   "fst (1, true)",
   Fst (Pair (Num 1, True)));

  ("Snd",
   "snd (1, true)",
   Snd (Pair (Num 1, True)));

  (* Unit *)
  ("Unit",
   "unit",
   Eunit);

  (* Sum types *)
  ("Inl",
   "inl 3 as int+bool",
   Inl (Num 3));

  ("Inr",
   "inr false as int+bool",
   Inr False);

  ("Case Inl",
   "case inl 3 as int+bool of inl x => x | inr y => 0",
   Case (
     Inl (Num 3),
     Lam (Ind 0),
     Lam (Num 0)
   ));

  ("Case Inr",
   "case inr false as int+bool of inl x => 1 | inr y => y",
   Case (
     Inr False,
     Lam (Num 1),
     Lam (Ind 0)
   ));

  (* Fix *)
  ("Fix Identity",
   "fix f:int->int => fn x:int => x",
   Fix (Lam (Ind 0)));

  (* Boolean *)
  ("True",
   "true",
   True);

  ("False",
   "false",
   False);

  ("If-then-else",
   "if a then b else c a",
   Ifthenelse (
     Ind 0,
     Ind 2,
     App (Ind 1, Ind 0)
   ));

  ("If Constant",
   "if true then 1 else 0",
   Ifthenelse (
     True,
     Num 1,
     Num 0
   ));

  (* Numbers *)
  ("Number",
   "42",
   Num 42);

  (* Arithmetic *)
  ("Plus",
   "+ (1, 2)",
   App (App (Plus, Num 1), Num 2));

  ("Minus",
   "- (5, 3)",
   App (App (Minus, Num 5), Num 3));

  ("Eq True",
   "= (4, 4)",
   App (App (Eq, Num 4), Num 4));

  ("Eq False",
   "= (4, 5)",
   App (App (Eq, Num 4), Num 5))
]

let run_test (name, input, expected) =
  try
    let texp = parse_str input in
    let exp = Eval.texp2exp texp in
    if exp = expected then begin
      Printf.printf "[PASS] %s\n" name; true
    end else begin
      Printf.printf "[FAIL] %s\n       expected: %s\n       output: %s\n" 
        name (Eval.exp2string expected) (Eval.exp2string exp); false
    end
  with
  | Eval.NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

let () =
  Printf.printf "\n============= [Part 1] De Bruijn Indexes 테스트 =============\n";
  let pass_count = List.fold_left (fun acc tc -> if run_test tc then acc + 1 else acc) 0 test_cases in
  Printf.printf "============= 결과: [%d/%d] pass =============\n\n" pass_count (List.length test_cases)