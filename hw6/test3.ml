open Tml
open Eval

let parse_str s = 
  let s = s ^ ";" in 
  Parser.parse Lexer.token (Lexing.from_string s)

type test_case = string * string * string

(* 주의: value2exp가 (), true, false 만 지원하므로 결과가 V_prim 인 케이스만 테스트합니다. *)
let test_cases = [
  ("CBNeed Basic Bool", "if true then false else true", "false");
  ("CBNeed Pair", "fst (true, false)", "true");
  ("CBNeed Arithmetic Eq", "= (3, 3)", "true");
  ("CBNeed App", "(fn x:bool => x) false", "false");
  
  (* Call-By-Need 핵심 테스트: 인자가 평가될 필요가 없으면 무한루프(Fix)라도 정상 종료되어야 함 *)
  ("CBNeed Lazy Evaluation (Fix)", "(fn x:int => true) (fix f:int => f)", "true");
]

let run_test (name, input, expected_str) =
  try
    let exp = Eval.texp2exp (parse_str input) in
    let expected = Eval.texp2exp (parse_str expected_str) in
    
    let initial_st = Eval.Anal_ST(Heap.empty, Eval.Hole_SK, exp, Eval.emptyEnv) in
    let final_st = Eval.multiStep2 initial_st in
    
    match final_st with
    | Eval.Return_ST(_, Eval.Hole_SK, v) ->
        let result_exp = Eval.value2exp v in
        if result_exp = expected then begin
          Printf.printf "[PASS] %s\n" name; true
        end else begin
          Printf.printf "[FAIL] %s\n       expected: %s\n       output: %s\n" 
            name (Eval.exp2string expected) (Eval.exp2string result_exp); false
        end
    | _ -> Printf.printf "[FAIL] %s (Did not terminate in valid Return_ST)\n" name; false
  with
  | Eval.NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
  | Eval.NotConvertible -> Printf.printf "[FAIL] %s (NotConvertible Exception raised! Check value2exp)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

let () =
  Printf.printf "\n============= [Part 3] Call-By-Need (Abstract Machine N) 테스트 =============\n";
  let pass_count = List.fold_left (fun acc tc -> if run_test tc then acc + 1 else acc) 0 test_cases in
  Printf.printf "============= 결과: [%d/%d] pass =============\n\n" pass_count (List.length test_cases)