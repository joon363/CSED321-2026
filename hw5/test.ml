open Tml
open Typing

(* 테스트 케이스 타입 정의 *)
type test_case = 
  | TypeTest of string * string * Tml.tp  (* 이름, 입력 수식, 예상되는 타입 *)
  | ErrorTest of string * string          (* 이름, 입력 수식 (TypeError가 나야 함) *)

let test_cases = ref []

let add_type_test name input expected = test_cases := !test_cases @ [TypeTest(name, input, expected)]
let add_error_test name input = test_cases := !test_cases @ [ErrorTest(name, input)]

(* 문자열을 받아 Tml.exp로 파싱하는 유틸리티 *)
let parse_str s = 
  let s = s ^ ";" in (* Parser requires EOF mapped to ';' *)
  Parser.parse Lexer.token (Lexing.from_string s)

(* 정상 타입 추론 테스트 실행 *)
let run_type_test name input expected =
  try
    let e = parse_str input in
    let t = Typing.typeOf e in
    if t = expected then begin
      Printf.printf "[PASS] %s\n" name; true
    end else begin
      Printf.printf "[FAIL] %s\n       expected: %s\n       output: %s\n" 
        name (Tml.tp2string expected) (Tml.tp2string t);
      false
    end
  with
  | Typing.TypeError -> Printf.printf "[FAIL] %s (TypeError raised)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

(* 타입 에러(TypeError) 발생 테스트 실행 *)
let run_error_test name input =
  try
    let e = parse_str input in
    let t = Typing.typeOf e in
    Printf.printf "[FAIL] %s (Should raise TypeError but inferred %s)\n" name (Tml.tp2string t); false
  with
  | Typing.TypeError -> Printf.printf "[PASS] %s\n" name; true
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

let () =
  (* 1. Basic Types (Unit, Bool, Int) *)
  add_type_test "Basic - Unit" "()" Unit;
  add_type_test "Basic - True" "true" Bool;
  add_type_test "Basic - False" "false" Bool;
  add_type_test "Basic - Num" "42" Int;

  (* 2. Operators *)
  add_type_test "Op - Plus" "+" (Fun (Prod (Int, Int), Int));
  add_type_test "Op - Minus" "-" (Fun (Prod (Int, Int), Int));
  add_type_test "Op - Eq" "=" (Fun (Prod (Int, Int), Bool));
  add_type_test "Op - Plus App" "+ (1, 2)" Int;
  add_type_test "Op - Eq App" "= (1, 1)" Bool;

  (* 3. If-then-else *)
  add_type_test "If - Normal" "if true then 1 else 2" Int;
  add_type_test "If - Nested" "if (= (1, 2)) then true else false" Bool;
  add_error_test "If Error - Cond not bool" "if 1 then 2 else 3";
  add_error_test "If Error - Branch mismatch" "if true then 1 else false";

  (* 4. Pairs (Prod) *)
  add_type_test "Pair - Normal" "(1, true)" (Prod (Int, Bool));
  add_type_test "Pair - Fst" "fst (1, true)" Int;
  add_type_test "Pair - Snd" "snd (1, true)" Bool;
  add_error_test "Pair Error - Fst on non-pair" "fst 1";
  add_error_test "Pair Error - Snd on non-pair" "snd true";

  (* 5. Functions & Application (Lam, App) *)
  add_type_test "Fun - Id" "fn x : int => x" (Fun (Int, Int));
  add_type_test "Fun - App" "(fn x : int => x) 5" Int;
  add_type_test "Fun - Higher Order" "fn f : int -> bool => fn x : int => f x" (Fun (Fun (Int, Bool), Fun (Int, Bool)));
  add_error_test "App Error - Not a function" "1 2";
  add_error_test "App Error - Arg type mismatch" "(fn x : int => x) true";

  (* 6. Sum Types (Inl, Inr, Case) *)
  add_type_test "Sum - Inl" "inl (bool) 1" (Sum (Int, Bool));
  add_type_test "Sum - Inr" "inr (int) true" (Sum (Int, Bool));
  add_type_test "Sum - Case" "case inl (bool) 1 of inl a => true | inr b => b" Bool;
  add_error_test "Sum Error - Case on non-sum" "case 1 of inl a => true | inr b => false";
  add_error_test "Sum Error - Case branch mismatch" "case inl (bool) 1 of inl a => 1 | inr b => false";

  (* 7. Fixpoint (Fix) *)
  add_type_test "Fix - Normal" "fix x : int => x" Int;
  add_error_test "Fix Error - Type mismatch" "fix x : int => true";

  (* 8. Unbound Variable *)
  add_error_test "Free Variable" "x";

  (* 9. Syntactic Sugar (Let, Let rec) & Complex Examples *)
  add_type_test "Sugar - Let" "let x : int = 1 in x" Int;
  add_type_test "Sugar - Let (Function)" "let f : int -> int = fn x : int => x in f" (Fun (Int, Int));
  add_type_test "Sugar - Let Rec" "let rec f : int -> int = fn x : int => f x in f 10" Int;
  
  (* test.tml Examples Validation *)
  add_type_test "test.tml - fst (1, true)" "fst (1, true)" Int;
  add_type_test "test.tml - case inl" "case inl (int) 10 of inl a => + (a, 5) | inr b => - (b, 5)" Int;
  add_type_test "test.tml - fn x: unit+unit" "(fn x : unit + unit => case x of inl a => true | inr a => false) (inl (unit) ())" Bool;

  let total = List.length !test_cases in
  let pass_count = ref 0 in
  
  Printf.printf "\n============= CSED-321 HW5 테스트 결과 =============\n";
  
  List.iter (fun tc ->
    let passed = match tc with
      | TypeTest (name, input, expected) -> run_type_test name input expected
      | ErrorTest (name, input) -> run_error_test name input
    in
    if passed then incr pass_count
  ) !test_cases;

  Printf.printf "\n============= 최종 결과 =============\n";
  Printf.printf "[%d/%d] pass\n\n" !pass_count total