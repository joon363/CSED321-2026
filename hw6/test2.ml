open Tml
open Eval

let parse_str s = 
  let s = s ^ ";" in 
  Parser.parse Lexer.token (Lexing.from_string s)

type test_case = string * string * string

let test_cases = [
  ("CBV Pair/Fst", "fst (1, 2)", "1");
  ("CBV Pair/Snd", "snd (1, 2)", "2");
  ("CBV Arithmetic", "+ (5, 3)", "8");
  ("CBV If-then-else", "if true then 1 else 2", "1");
  ("CBV App/Lam", "(fn x:int => x) 5", "5");
  ("CBV Complex App", "(fn x:int => fn y:int => + (x, y)) 1 2", "3");
  ("CBV Case", "case inl (int) 5 of inl a => + (a, 1) | inr b => 0", "6");
]

let run_test (name, input, expected_str) =
  try
    let exp = Eval.texp2exp (parse_str input) in
    let expected = Eval.texp2exp (parse_str expected_str) in
    let result = Eval.multiStep1 exp in
    if result = expected then begin
      Printf.printf "[PASS] %s\n" name; true
    end else begin
      Printf.printf "[FAIL] %s\n       expected: %s\n       output: %s\n" 
        name (Eval.exp2string expected) (Eval.exp2string result); false
    end
  with
  | Eval.NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

let () =
  Printf.printf "\n============= [Part 2] Call-By-Value 테스트 =============\n";
  let pass_count = List.fold_left (fun acc tc -> if run_test tc then acc + 1 else acc) 0 test_cases in
  Printf.printf "============= 결과: [%d/%d] pass =============\n\n" pass_count (List.length test_cases)