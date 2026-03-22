open Hw1

(* 문자열 변환 함수들 (에러 출력용) *)
let string_of_int_list lst = "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"
let string_of_bool b = if b then "true" else "false"

let rec string_of_list string_of_elem lst = 
  "[" ^ String.concat "; " (List.map string_of_elem lst) ^ "]"

let string_of_int_pair (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(* 테스트 케이스 타입 정의: 이름, 실행 함수, 문자열 변환 함수(정답/결과 출력용) *)
type test_case = 
  | IntTest of string * (unit -> int) * int
  | BoolTest of string * (unit -> bool) * bool
  | IntListTest of string * (unit -> int list) * int list
  | IntPairListTest of string * (unit -> (int * int) list) * (int * int) list
  | IntListListTest of string * (unit -> int list list) * int list list
  | CustomTest of string * (unit -> bool) (* 복잡한 비교(예: depth, split)는 기존처럼 bool 반환 *)

let test_cases = ref []

let add_int_test name f expected = test_cases := !test_cases @ [IntTest(name, f, expected)]
let add_bool_test name f expected = test_cases := !test_cases @ [BoolTest(name, f, expected)]
let add_int_list_test name f expected = test_cases := !test_cases @ [IntListTest(name, f, expected)]
let add_int_pair_list_test name f expected = test_cases := !test_cases @ [IntPairListTest(name, f, expected)]
let add_int_list_list_test name f expected = test_cases := !test_cases @ [IntListListTest(name, f, expected)]
let add_custom_test name f = test_cases := !test_cases @ [CustomTest(name, f)]

(* 복잡한 결과값의 순서 무관 비교를 위한 헬퍼 함수 *)
let sort_lst lst = List.sort compare lst
let sort_lol lst = List.sort compare (List.map sort_lst lst)
let sort_pairs lst = List.sort compare lst

let _ =
  (* 1. fac *)
  add_int_test "fac 1" (fun () -> fac 1) 1;
  add_int_test "fac 5" (fun () -> fac 5) 120;

  (* 2. power *)
  add_int_test "power 2 0" (fun () -> power 2 0) 1;
  add_int_test "power 2 3" (fun () -> power 2 3) 8;
  add_int_test "power -3 1" (fun () -> power (-3) 1) (-3);

  (* 3. gcd *)
  add_int_test "gcd 10 5" (fun () -> gcd 10 5) 5;
  add_int_test "gcd 14 21" (fun () -> gcd 14 21) 7;
  add_int_test "gcd 81 36" (fun () -> gcd 81 36) 9;
  add_int_test "gcd 132 108" (fun () -> gcd 132 108) 12;
  add_int_test "gcd 100 0" (fun () -> gcd 100 0) 100;
  add_int_test "gcd 0 100" (fun () -> gcd 0 100) 100;

  (* 4. combi *)
  add_int_test "combi 5 0" (fun () -> combi 5 0) 1;
  add_int_test "combi 5 5" (fun () -> combi 5 5) 1;
  add_int_test "combi 5 2" (fun () -> combi 5 2) 10;

  (* 5. sum_tree *)
  add_int_test "sum_tree leaf" (fun () -> sum_tree (Leaf 3)) 3;
  add_int_test "sum_tree node" (fun () -> sum_tree (Node (Leaf 1, 2, Leaf 3))) 6;

  (* 6. depth *)
  add_custom_test "depth base" (fun () -> let d = depth (Leaf 3) in d = 0 || d = 1);
  add_custom_test "depth node" (fun () -> 
    let d_leaf = depth (Leaf 3) in
    depth (Node (Leaf 1, 2, Leaf 3)) = d_leaf + 1);
  add_custom_test "depth unbalanced" (fun () -> 
    let d_leaf = depth (Leaf 3) in
    depth (Node (Node (Leaf 1, 2, Leaf 3), 4, Leaf 5)) = d_leaf + 2);

  (* 7. bin_search *)
  add_bool_test "bin_search found" (fun () -> bin_search (Node (Leaf 1, 2, Leaf 3)) 2) true;
  add_bool_test "bin_search not found" (fun () -> bin_search (Node (Leaf 1, 2, Leaf 3)) 4) false;

  (* 8. inorder *)
  add_int_list_test "inorder" (fun () -> inorder (Node (Leaf 1, 2, Leaf 3))) [1; 2; 3];

  (* 9. max *)
  add_int_test "max1" (fun () -> max [1; 5; 3; 2]) 5;
  add_int_test "max2" (fun () -> max [1]) 1;
  add_int_test "max3" (fun () -> max []) 0;
  add_int_test "max4" (fun () -> max [(-1);(-1)]) (-1);

  (* 10. list_add *)
  add_int_list_test "list_add" (fun () -> list_add [1; 2] [3; 4]) [4; 6];
  add_int_list_test "list_add2" (fun () -> list_add [1; 2; 3] [3; 4]) [4; 6; 3];
  add_int_list_test "list_add3" (fun () -> list_add [] [3; 4]) [3; 4];

  (* 11. insert *)
  add_int_list_test "insert empty" (fun () -> insert 2 []) [2];
  add_int_list_test "insert one" (fun () -> insert 4 [2]) [2;4];
  add_int_list_test "insert mid" (fun () -> insert 2 [1; 3]) [1; 2; 3];
  add_int_list_test "insert head" (fun () -> insert 1 [2; 3]) [1; 2; 3];

  (* 12. insort *)
  add_int_list_test "insort" (fun () -> insort [3; 1; 4; 2]) [1; 2; 3; 4];

  (* 13. compose *)
  add_int_test "compose" (fun () -> compose (fun x -> x + 1) (fun x -> x * 2) 3) 8; 

  (* 14. curry *)
  add_int_test "curry" (fun () -> curry (fun (x, y) -> x + y) 1 2) 3;

  (* 15. uncurry *)
  add_int_test "uncurry" (fun () -> uncurry (fun x y -> x + y) (1, 2)) 3;

  (* 16. multifun *)
  add_int_test "multifun 1" (fun () -> multifun (fun x -> x * x) 1 3) 9;
  add_int_test "multifun 2" (fun () -> multifun (fun x -> x * x) 2 3) 81;

  (* 17. ltake *)
  add_int_list_test "ltake" (fun () -> ltake [1; 2; 3; 4] 2) [1; 2];
  add_int_list_test "ltake empty" (fun () -> ltake [] 2) [];

  (* 18. lexists *)
  add_bool_test "lexists true" (fun () -> lexists (fun x -> x > 2) [1; 2; 3]) true;
  add_bool_test "lexists false" (fun () -> lexists (fun x -> x > 5) [1; 2; 3]) false;

  (* 19. lmap *)
  add_int_list_test "lmap" (fun () -> lmap (fun x -> x + 1) [1; 2; 3]) [2; 3; 4];
  add_int_list_test "lmap empty" (fun () -> lmap (fun x -> x + 1) []) [];

  (* 20. lrev *)
  add_int_list_test "lrev" (fun () -> lrev [1; 2; 3]) [3; 2; 1];

  (* 21. lflat *)
  add_int_list_test "lflat" (fun () -> lflat [[1; 2]; [3; 4]; [5]]) [1; 2; 3; 4; 5];

  (* 22. lzip *)
  add_int_pair_list_test "lzip" (fun () -> lzip [1; 2] [3; 4]) [(1, 3); (2, 4)];
  add_int_pair_list_test "lzip diff" (fun () -> lzip [1; 2] [3; 4; 5]) [(1, 3); (2, 4)];
  add_int_pair_list_test "lzip empty" (fun () -> lzip [1; 2] []) [];

  (* 23. split *)
  add_custom_test "split" (fun () ->
    let (l1, l2) = split [1; 2; 3; 4] in
    (l1 = [1; 3] && l2 = [2; 4])
  );
  add_custom_test "split none" (fun () ->
    let (l1, l2) = split [] in
    (l1 = [] && l2 = [])
  );
  add_custom_test "split one" (fun () ->
    let (l1, l2) = split [1] in
    (l1 = [1] && l2 = []) 
  );

  (* 24. cartprod *)
  add_int_pair_list_test "cartprod" (fun () -> sort_pairs (cartprod [1; 2] [3; 4])) 
    (sort_pairs [(1, 3); (1, 4); (2, 3); (2, 4)]);

  (* 25. powerset *)
  add_int_list_list_test "powerset empty" (fun () -> sort_lol (powerset [])) (sort_lol [[]]);
  add_int_list_list_test "powerset [1;2]" (fun () -> sort_lol (powerset [1; 2])) 
    (sort_lol [[]; [1]; [2]; [1; 2]]);
  
  add_int_list_list_test "powerset [1;2;3]" (fun () -> sort_lol (powerset [1; 2; 3])) 
    (sort_lol [[]; [1]; [2]; [3]; [1; 2];[1; 3];[2; 3];[1; 2; 3]])


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
  | Not_implemented -> Printf.printf "[FAIL] %s (Not_implemented)\n" name; false
  | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false

let () =
  let total = List.length !test_cases in
  let pass_count = ref 0 in
  
  Printf.printf "\n============= 테스트 결과 =============\n";
  
  List.iter (fun tc ->
    let passed = match tc with
      | IntTest (name, f, exp) -> run_test name string_of_int (=) f exp
      | BoolTest (name, f, exp) -> run_test name string_of_bool (=) f exp
      | IntListTest (name, f, exp) -> run_test name string_of_int_list (=) f exp
      | IntPairListTest (name, f, exp) -> run_test name (string_of_list string_of_int_pair) (=) f exp
      | IntListListTest (name, f, exp) -> run_test name (string_of_list string_of_int_list) (=) f exp
      | CustomTest (name, f) -> 
          try
            if f () then (Printf.printf "[PASS] %s\n" name; true)
            else (Printf.printf "[FAIL] %s\n       (Custom test failed, no specific output format)\n" name; false)
          with
          | Not_implemented -> Printf.printf "[FAIL] %s (Not_implemented)\n" name; false
          | e -> Printf.printf "[FAIL] %s (Exception: %s)\n" name (Printexc.to_string e); false
    in
    if passed then incr pass_count
  ) !test_cases;

  Printf.printf "\n============= 최종 결과 =============\n";
  Printf.printf "[%d/%d] pass\n\n" !pass_count total