open Common
open Hw3

(* 테스트를 위한 정수형 벡터 및 행렬 모듈 인스턴스화 *)
module IntVec = VectorFn(Integer)
module IntMat = MatrixFn(Integer)

(* 문자열 변환 함수들 (에러 출력용) *)
let string_of_int_list lst = "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"
let string_of_bool b = if b then "true" else "false"
let string_of_bool_list lst = "[" ^ String.concat "; " (List.map string_of_bool lst) ^ "]"

let rec string_of_list string_of_elem lst = 
  "[" ^ String.concat "; " (List.map string_of_elem lst) ^ "]"

let string_of_int_list_list lst = string_of_list string_of_int_list lst
let string_of_bool_list_list lst = string_of_list string_of_bool_list lst

(* 테스트 케이스 타입 정의 *)
type test_case = 
  | IntTest of string * (unit -> int) * int
  | BoolTest of string * (unit -> bool) * bool
  | IntListTest of string * (unit -> int list) * int list
  | IntListListTest of string * (unit -> int list list) * int list list
  | BoolListListTest of string * (unit -> bool list list) * bool list list
  | CustomTest of string * (unit -> bool)

let test_cases = ref []

let add_int_test name f expected = test_cases := !test_cases @ [IntTest(name, f, expected)]
let add_bool_test name f expected = test_cases := !test_cases @ [BoolTest(name, f, expected)]
let add_int_list_test name f expected = test_cases := !test_cases @ [IntListTest(name, f, expected)]
let add_int_list_list_test name f expected = test_cases := !test_cases @ [IntListListTest(name, f, expected)]
let add_bool_list_list_test name f expected = test_cases := !test_cases @ [BoolListListTest(name, f, expected)]
let add_custom_test name f = test_cases := !test_cases @ [CustomTest(name, f)]

let _ =
  (* 1. Boolean Module Tests *)
  add_bool_test "Boolean zero" (fun () -> Boolean.zero) false;
  add_bool_test "Boolean one" (fun () -> Boolean.one) true;
  add_bool_test "Boolean ++ (OR)" (fun () -> Boolean.(++) false true) true;
  add_bool_test "Boolean ++ (OR) edge" (fun () -> Boolean.(++) false false) false;
  add_bool_test "Boolean ** (AND)" (fun () -> Boolean.( ** ) true false) false;
  add_bool_test "Boolean ** (AND) edge" (fun () -> Boolean.( ** ) true true) true;
  add_bool_test "Boolean ==" (fun () -> Boolean.(==) true true) true;
  add_bool_test "Boolean == false" (fun () -> Boolean.(==) true false) false;

  (* 2. VectorFn Module Tests *)
  add_int_list_test "Vector create & to_list" (fun () -> IntVec.(to_list (create [1; 2; 3]))) [1; 2; 3];
  add_custom_test "Vector create empty" (fun () -> 
    try let _ = IntVec.create [] in false with IntVec.VectorIllegal -> true | _ -> false);
  add_int_test "Vector dim" (fun () -> IntVec.(dim (create [1; 2; 3; 4]))) 4;
  add_int_test "Vector nth normal" (fun () -> IntVec.(nth (create [10; 20; 30]) 1)) 20;
  add_custom_test "Vector nth out of bound" (fun () -> 
    try let _ = IntVec.(nth (create [1; 2]) 2) in false with IntVec.VectorIllegal -> true | _ -> false);
  add_custom_test "Vector nth negative" (fun () -> 
    try let _ = IntVec.(nth (create [1; 2]) (-1)) in false with IntVec.VectorIllegal -> true | _ -> false);
  add_int_list_test "Vector ++" (fun () -> IntVec.(to_list (create [1; 2] ++ create [3; 4]))) [4; 6];
  add_custom_test "Vector ++ diff dim" (fun () -> 
    try let _ = IntVec.(create [1] ++ create [2; 3]) in false with IntVec.VectorIllegal -> true | _ -> false);
  add_bool_test "Vector ==" (fun () -> IntVec.(create [1; 2] == create [1; 2])) true;
  add_bool_test "Vector == diff elem" (fun () -> IntVec.(create [1; 2] == create [2; 1])) false;
  add_custom_test "Vector == diff dim" (fun () -> 
    try let _ = IntVec.(create [1] == create [1; 2]) in false with IntVec.VectorIllegal -> true | _ -> false);
  add_int_test "Vector innerp" (fun () -> IntVec.(innerp (create [1; 2; 3]) (create [4; 5; 6]))) 32; (* 4 + 10 + 18 *)
  add_custom_test "Vector innerp diff dim" (fun () -> 
    try let _ = IntVec.(innerp (create [1]) (create [2; 3])) in false with IntVec.VectorIllegal -> true | _ -> false);

  (* 3. MatrixFn Module Tests *)
  add_int_list_list_test "Matrix create & to_list" (fun () -> IntMat.(to_list (create [[1; 2]; [3; 4]]))) [[1; 2]; [3; 4]];
  add_custom_test "Matrix create empty" (fun () -> 
    try let _ = IntMat.create [] in false with IntMat.MatrixIllegal -> true | _ -> false);
  add_custom_test "Matrix create non-square 1" (fun () -> 
    try let _ = IntMat.create [[1; 2; 3]; [4; 5; 6]] in false with IntMat.MatrixIllegal -> true | _ -> false);
  add_custom_test "Matrix create jagged" (fun () -> 
    try let _ = IntMat.create [[1; 2]; [3]] in false with IntMat.MatrixIllegal -> true | _ -> false);
  add_int_list_list_test "Matrix identity" (fun () -> IntMat.(to_list (identity 3))) [[1; 0; 0]; [0; 1; 0]; [0; 0; 1]];
  add_custom_test "Matrix identity invalid dim" (fun () -> 
    try let _ = IntMat.identity 0 in false with IntMat.MatrixIllegal -> true | _ -> false);
  add_int_test "Matrix dim" (fun () -> IntMat.(dim (create [[1; 2]; [3; 4]]))) 2;
  add_int_list_list_test "Matrix transpose" (fun () -> IntMat.(to_list (transpose (create [[1; 2]; [3; 4]])))) [[1; 3]; [2; 4]];
  add_int_test "Matrix get" (fun () -> IntMat.(get (create [[1; 2]; [3; 4]]) 1 0)) 3;
  add_custom_test "Matrix get out of bounds" (fun () -> 
    try let _ = IntMat.(get (create [[1]]) 1 1) in false with IntMat.MatrixIllegal -> true | _ -> false);
  add_int_list_list_test "Matrix ++" (fun () -> IntMat.(to_list (create [[1; 2]; [3; 4]] ++ create [[5; 6]; [7; 8]]))) [[6; 8]; [10; 12]];
  add_custom_test "Matrix ++ diff dim" (fun () -> 
    try let _ = IntMat.(create [[1]] ++ create [[1; 2]; [3; 4]]) in false with IntMat.MatrixIllegal -> true | _ -> false);
  add_int_list_list_test "Matrix **" (fun () -> IntMat.(to_list (create [[1; 2]; [3; 4]] ** create [[2; 0]; [1; 2]]))) [[4; 4]; [10; 8]];
  add_custom_test "Matrix ** diff dim" (fun () -> 
    try let _ = IntMat.(create [[1]] ** create [[1; 2]; [3; 4]]) in false with IntMat.MatrixIllegal -> true | _ -> false);
  add_bool_test "Matrix ==" (fun () -> IntMat.(create [[1; 2]; [3; 4]] == create [[1; 2]; [3; 4]])) true;
  add_bool_test "Matrix == false" (fun () -> IntMat.(create [[1; 2]; [3; 4]] == create [[4; 3]; [2; 1]])) false

  (* 4. ClosureFn Module Tests *)
  (* add_custom_test "Closure calculation 1" (fun () -> 
    let m = BoolMat.create [[false; true]; [false; false]] in
    let expected = BoolMat.create [[true; true]; [false; true]] in
    BoolMat.(BoolMatClosure.closure m == expected)
  );
  add_custom_test "Closure calculation 2 (Identity)" (fun () -> 
    let m = BoolMat.identity 2 in
    BoolMat.(BoolMatClosure.closure m == m)
  );

  (* 5. Reachability Problem Tests *)
  add_bool_list_list_test "reach normal (al)" (fun () -> reach al) solution_al';
  add_custom_test "reach IllegalFormat empty" (fun () -> 
    try let _ = reach [] in false with IllegalFormat -> true | _ -> false);
  add_custom_test "reach IllegalFormat non-square" (fun () -> 
    try let _ = reach [[true; false]; [true; true; false]] in false with IllegalFormat -> true | _ -> false) *)



(* 메인 실행부 *)
let run_test name to_string comp f expected =
  try
    let result = f () in
    if comp result expected then begin
      Printf.printf "[PASS] %s\n" name;
      true
    end else begin
      Printf.printf "[FAIL] %s\n       answer: %s\n       output: %s\n" 
        name (to_string expected) (to_string result);
      false
    end
  with
  | NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

let () =
  let total = List.length !test_cases in
  let pass_count = ref 0 in
  
  Printf.printf "\n============= CSED-321 HW3 테스트 결과 =============\n";
  
  List.iter (fun tc ->
    let passed = match tc with
      | IntTest (name, f, exp) -> run_test name string_of_int (=) f exp
      | BoolTest (name, f, exp) -> run_test name string_of_bool (=) f exp
      | IntListTest (name, f, exp) -> run_test name string_of_int_list (=) f exp
      | IntListListTest (name, f, exp) -> run_test name string_of_int_list_list (=) f exp
      | BoolListListTest (name, f, exp) -> run_test name string_of_bool_list_list (=) f exp
      | CustomTest (name, f) -> 
          try
            if f () then (Printf.printf "[PASS] %s\n" name; true)
            else (Printf.printf "[FAIL] %s\n       (Custom test failed, check your module logic)\n" name; false)
          with
          | NotImplemented -> Printf.printf "[FAIL] %s (NotImplemented)\n" name; false
          | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false
    in
    if passed then incr pass_count
  ) !test_cases;

  Printf.printf "\n============= 최종 결과 =============\n";
  Printf.printf "[%d/%d] pass\n\n" !pass_count total