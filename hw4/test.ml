open Uml
open Eval
open Inout

(* 테스트 케이스 타입 정의 *)
type test_case = 
  | StepTest of string * string * string (* 이름, 입력 수식, 1-step reduction 후 예상되는 수식 *)
  | StuckTest of string * string         (* 이름, 입력 수식 (Stuck이 나야 함) *)
  | FileTest of string * string          (* 이름, 파일 경로 *)

let test_cases = ref []

let add_step_test name input expected = test_cases := !test_cases @ [StepTest(name, input, expected)]
let add_stuck_test name input = test_cases := !test_cases @ [StuckTest(name, input)]
let add_file_test name filepath = test_cases := !test_cases @ [FileTest(name, filepath)]

(* 문자열을 받아 Uml.exp로 파싱하는 유틸리티 *)
let parse_str s = 
  let s = s ^ ";" in (* Parser requires EOF which is mapped to ';' in lexer *)
  Parser.parse Lexer.token (Lexing.from_string s)

(* Alpha-equivalence 비교를 위한 De Bruijn Index 변환기 *)
let to_debruijn e =
  let rec aux env e =
    match e with
    | Uml.Var x -> 
        let rec find idx = function
          | [] -> Uml.Var x (* Free variable *)
          | h::t -> if h = x then Uml.Var (string_of_int idx) else find (idx + 1) t
        in find 0 env
    | Uml.Lam (x, e') ->
        Uml.Lam ("", aux (x::env) e')
    | Uml.App (e1, e2) ->
        Uml.App (aux env e1, aux env e2)
  in aux [] e

(* 변수명과 관계없이 두 AST가 구조적으로 동일한 의미인지 확인 *)
let alpha_eq e1 e2 = 
  (to_debruijn e1) = (to_debruijn e2)

(* 1-step reduction 테스트 실행 *)
let run_step_test name input expected_str =
  try
    let e = parse_str input in
    let expected = parse_str expected_str in
    let e' = Eval.stepv e in
    if alpha_eq e' expected then begin
      Printf.printf "[PASS] %s\n" name; true
    end else begin
      Printf.printf "[FAIL] %s\n       expected: %s\n       output: %s\n" 
        name (Inout.exp2string expected) (Inout.exp2string e');
      false
    end
  with
  | Eval.NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

(* Stuck 테스트 실행 (더 이상 reduction 불가능해야 함) *)
let run_stuck_test name input =
  try
    let e = parse_str input in
    let _ = Eval.stepv e in
    Printf.printf "[FAIL] %s (Should be stuck but reduced successfully)\n" name; false
  with
  | Eval.Stuck -> Printf.printf "[PASS] %s\n" name; true
  | Eval.NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

(* UML 파일 전체 실행 테스트 (에러 없이 multiStep을 통해 끝까지 reduction 되는지 검사) *)
let run_file_test name filepath =
  try
    let seq = Inout.read_file filepath in
    Seq.iter (fun e -> let _ = Eval.multiStep Eval.stepv e in ()) seq;
    Printf.printf "[PASS] %s\n" name; true
  with
  | Eval.NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

let () =
  (* 1. Call-By-Value Rules Tests *)
  add_step_test "Subst Failed 1" "((lam x. y) x)" "y";
  add_step_test "Subst Failed 2" "((lam x. x) x)" "x";
  add_step_test "Subst Failed 3" "(((lam x. x)(lam x. y)) x)" "(((lam x.y) x))";
  add_step_test "Subst Failed 3-1" "(((lam x.y) x))" "y";
  add_step_test "Subst Var Hit" "((lam x. x) (lam y. y))" "(lam y. y)";
  add_step_test "Subst Var Miss" "((lam x. z) (lam y. y))" "z";
  add_step_test "Subst App" "((lam x. (x x)) (lam y. y))" "((lam y. y) (lam y. y))";
  add_step_test "Subst Shadowing" "((lam x. (lam x. x)) (lam y. y))" "(lam x. x)";
  add_step_test "Subst No Capture" "((lam x. (lam y. x)) (lam z. z))" "(lam y. (lam z. z))";
  add_step_test "Subst Alpha Rename" "((lam x. (lam y. (x y))) y)" "(lam z. (y z))";
  add_step_test "CBV Lam Rule" "((lam x. x) (lam y. y)) (lam z. z)" "(lam y. y) (lam z. z)";
  add_step_test "CBV Arg Rule" "(lam x. x) ((lam y. y) (lam z. z))" "(lam x. x) (lam z. z)";
  add_step_test "CBV App Rule (Beta Reduction)" "(lam x. x) (lam y. y)" "lam y. y";
  add_step_test "CBV App Rule (Beta Reduction - bool)" "(lam t. (lam f. t)) (lam x. x) (lam y. y)" "(lam f. (lam x.x)) (lam y.y)";
  add_step_test "CBV App Rule (Substitution)" "(lam x. lam y. x) (lam a. a)" "lam y. (lam a. a)";
  add_step_test "CBV Complex Order" "((lam x. x) (lam x. x)) ((lam y. y) (lam y. y))" "(lam x. x) ((lam y. y) (lam y. y))";
  
  (* 2. Stuck Exception Tests *)
  add_stuck_test "Stuck - Variable" "x";
  add_stuck_test "Stuck - Value (Lambda)" "lam x. x";
  add_stuck_test "Stuck - Free Variable App" "x (lam y. y)";
  add_stuck_test "Stuck - App" "((lam x.x) y) x";
  
  (* 3. Example Files Executions Tests *)
  add_file_test "File - nat.uml" "example/nat.uml";
  add_file_test "File - rec.uml" "example/rec.uml";
  add_file_test "File - fib.uml" "example/fib.uml";

  let total = List.length !test_cases in
  let pass_count = ref 0 in
  
  Printf.printf "\n============= CSED-321 HW4 테스트 결과 =============\n";
  
  List.iter (fun tc ->
    let passed = match tc with
      | StepTest (name, input, expected) -> run_step_test name input expected
      | StuckTest (name, input) -> run_stuck_test name input
      | FileTest (name, filepath) -> run_file_test name filepath
    in
    if passed then incr pass_count
  ) !test_cases;

  Printf.printf "\n============= 최종 결과 =============\n";
  Printf.printf "[%d/%d] pass\n\n" !pass_count total