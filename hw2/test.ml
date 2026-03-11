open Hw2

(* 문자열 변환 함수들 (에러 출력용) *)
let string_of_int_list lst = "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"
let string_of_bool b = if b then "true" else "false"

let rec string_of_list string_of_elem lst = 
  "[" ^ String.concat "; " (List.map string_of_elem lst) ^ "]"

let string_of_int_list_list lst = string_of_list string_of_int_list lst

(* 테스트 케이스 타입 정의: 이름, 실행 함수, 문자열 변환 함수(정답/결과 출력용) *)
type test_case = 
  | IntTest of string * (unit -> int) * int
  | BoolTest of string * (unit -> bool) * bool
  | IntListTest of string * (unit -> int list) * int list
  | IntListListTest of string * (unit -> int list list) * int list list
  | CustomTest of string * (unit -> bool)

let test_cases = ref []

let add_int_test name f expected = test_cases := !test_cases @ [IntTest(name, f, expected)]
let add_bool_test name f expected = test_cases := !test_cases @ [BoolTest(name, f, expected)]
let add_int_list_test name f expected = test_cases := !test_cases @ [IntListTest(name, f, expected)]
let add_int_list_list_test name f expected = test_cases := !test_cases @ [IntListListTest(name, f, expected)]
let add_custom_test name f = test_cases := !test_cases @ [CustomTest(name, f)]

(* 복잡한 결과값의 순서 무관 비교를 위한 헬퍼 함수 *)
let sort_lst lst = List.sort compare lst

let _ =
  (* 1. lrevrev *)
  add_int_list_list_test "lrevrev normal" (fun () -> lrevrev [[1; 2; 3]; [4; 5; 6]; [7]]) [[7]; [6; 5; 4]; [3; 2; 1]];
  add_int_list_list_test "lrevrev empty" (fun () -> lrevrev []) [];
  add_int_list_list_test "lrevrev empty inner" (fun () -> lrevrev [[]; [1; 2]; []]) [[]; [2; 1]; []];

  (* 2. lfoldr *)
  add_int_test "lfoldr sum" (fun () -> lfoldr (fun (x, acc) -> x + acc) 0 [1; 2; 4]) 7;
  add_int_test "lfoldr sub" (fun () -> lfoldr (fun (x, acc) -> x - acc) 0 [1; 2; 3]) 2;
  add_int_test "lfoldr empty" (fun () -> lfoldr (fun (x, acc) -> x + acc) 0 []) 0;
  add_int_list_test "lfoldr cons" (fun () -> lfoldr (fun (x, acc) -> x :: acc) [] [1; 2; 3]) [1; 2; 3];

  (* 3. fact *)
  add_int_test "fact 0" (fun () -> fact 0) 1;
  add_int_test "fact 1" (fun () -> fact 1) 1;
  add_int_test "fact 5" (fun () -> fact 5) 120;

  (* 4. fib *)
  add_int_test "fib 0" (fun () -> fib 0) 1;
  add_int_test "fib 1" (fun () -> fib 1) 1;
  add_int_test "fib 2" (fun () -> fib 2) 2;
  add_int_test "fib 5" (fun () -> fib 5) 8;

  (* 5. asum *)
  add_int_test "asum empty" (fun () -> asum []) 0;
  add_int_test "asum single" (fun () -> asum [1]) 1;
  add_int_test "asum normal" (fun () -> asum [3; 2; 7; 3]) 5;
  add_int_test "asum negative" (fun () -> asum [10; -5; 3]) 18;

  (* 6. ltabulate *)
  add_int_list_test "ltabulate 0" (fun () -> ltabulate 0 (fun x -> x)) [];
  add_int_list_test "ltabulate normal" (fun () -> ltabulate 4 (fun x -> x * x)) [0; 1; 4; 9];

  (* 7. lfilter *)
  add_int_list_test "lfilter normal" (fun () -> lfilter (fun x -> x > 2) [0; 1; 2; 3; 4; 5]) [3; 4; 5];
  add_int_list_test "lfilter all" (fun () -> lfilter (fun _ -> true) [1; 2]) [1; 2];
  add_int_list_test "lfilter none" (fun () -> lfilter (fun _ -> false) [1; 2]) [];
  add_int_list_test "lfilter empty" (fun () -> lfilter (fun x -> x > 0) []) [];

  (* 8. union *)
  add_custom_test "union normal" (fun () -> sort_lst (union [1; 2; 3] [2; 4; 6]) = [1; 2; 3; 4; 6]);
  add_custom_test "union empty 1" (fun () -> sort_lst (union [] [1; 2]) = [1; 2]);
  add_custom_test "union empty 2" (fun () -> sort_lst (union [1; 2] []) = [1; 2]);
  add_custom_test "union empty both" (fun () -> sort_lst (union [] []) = []);

  (* 9-11. Tree Traversals Prep *)
  let t_normal = Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4) in
  let t_empty_like = Leaf 5 in

  (* 9. inorder *)
  add_int_list_test "inorder normal" (fun () -> inorder t_normal) [1; 3; 2; 7; 4];
  add_int_list_test "inorder leaf" (fun () -> inorder t_empty_like) [5];

  (* 10. postorder *)
  add_int_list_test "postorder normal" (fun () -> postorder t_normal) [1; 2; 3; 4; 7];
  add_int_list_test "postorder leaf" (fun () -> postorder t_empty_like) [5];

  (* 11. preorder *)
  add_int_list_test "preorder normal" (fun () -> preorder t_normal) [7; 3; 1; 2; 4];
  add_int_list_test "preorder leaf" (fun () -> preorder t_empty_like) [5];

  (* 12. quicksort *)
  add_int_list_test "quicksort normal" (fun () -> quicksort [3; 7; 5; 1; 2]) [1; 2; 3; 5; 7];
  add_int_list_test "quicksort empty" (fun () -> quicksort []) [];
  add_int_list_test "quicksort single" (fun () -> quicksort [1]) [1];
  add_int_list_test "quicksort duplicates" (fun () -> quicksort [5; 5; 3; 5]) [3; 5; 5; 5];
  add_int_list_test "quicksort reverse" (fun () -> quicksort [5; 4; 3; 2; 1]) [1; 2; 3; 4; 5];

  (* 13. mergesort *)
  add_int_list_test "mergesort normal" (fun () -> mergesort [3; 7; 5; 1; 2]) [1; 2; 3; 5; 7];
  add_int_list_test "mergesort empty" (fun () -> mergesort []) [];
  add_int_list_test "mergesort single" (fun () -> mergesort [1]) [1];
  add_int_list_test "mergesort duplicates" (fun () -> mergesort [5; 5; 3; 5]) [3; 5; 5; 5];
  add_int_list_test "mergesort reverse" (fun () -> mergesort [5; 4; 3; 2; 1]) [1; 2; 3; 4; 5];

  (* 14. Heap *)
  add_custom_test "Heap basic allocate & dereference" (fun () ->
    let h0 = Heap.empty () in
    let (h1, l1) = Heap.allocate h0 10 in
    Heap.dereference h1 l1 = 10
  );
  add_custom_test "Heap update" (fun () ->
    let h0 = Heap.empty () in
    let (h1, l1) = Heap.allocate h0 10 in
    let h2 = Heap.update h1 l1 20 in
    Heap.dereference h2 l1 = 20
  );
  add_custom_test "Heap multiple variables" (fun () ->
    let h0 = Heap.empty () in
    let (h1, l1) = Heap.allocate h0 1 in
    let (h2, l2) = Heap.allocate h1 2 in
    Heap.dereference h2 l1 = 1 && Heap.dereference h2 l2 = 2
  );
  add_custom_test "Heap InvalidLocation (update)" (fun () ->
    let h0 = Heap.empty () in
    let (_, l1) = Heap.allocate h0 10 in
    try 
      let _ = Heap.update (Heap.empty ()) l1 20 in false 
    with Heap.InvalidLocation -> true | _ -> false
  );
  add_custom_test "Heap InvalidLocation (dereference)" (fun () ->
    let h0 = Heap.empty () in
    let (_, l1) = Heap.allocate h0 10 in
    try 
      let _ = Heap.dereference (Heap.empty ()) l1 in false 
    with Heap.InvalidLocation -> true | _ -> false
  );

  (* 15. DictList *)
  add_custom_test "DictList empty lookup" (fun () ->
    let d0 = DictList.empty () in
    DictList.lookup d0 "a" = None
  );
  add_custom_test "DictList insert & lookup" (fun () ->
    let d0 = DictList.empty () in
    let d1 = DictList.insert d0 ("a", 1) in
    DictList.lookup d1 "a" = Some 1
  );
  add_custom_test "DictList update existing" (fun () ->
    let d0 = DictList.empty () in
    let d1 = DictList.insert d0 ("a", 1) in
    let d2 = DictList.insert d1 ("a", 2) in
    DictList.lookup d2 "a" = Some 2
  );
  add_custom_test "DictList delete existing" (fun () ->
    let d0 = DictList.empty () in
    let d1 = DictList.insert d0 ("a", 1) in
    let d2 = DictList.delete d1 "a" in
    DictList.lookup d2 "a" = None
  );
  add_custom_test "DictList delete non-existing" (fun () ->
    let d0 = DictList.empty () in
    let d1 = DictList.insert d0 ("a", 1) in
    let d2 = DictList.delete d1 "b" in
    DictList.lookup d2 "a" = Some 1 && DictList.lookup d2 "b" = None
  );
  add_custom_test "DictList multiple keys" (fun () ->
    let d0 = DictList.empty () in
    let d1 = DictList.insert d0 ("a", 1) in
    let d2 = DictList.insert d1 ("b", 2) in
    DictList.lookup d2 "a" = Some 1 && DictList.lookup d2 "b" = Some 2
  );

  (* 16. DictFun *)
  add_custom_test "DictFun empty lookup" (fun () ->
    let d0 = DictFun.empty () in
    DictFun.lookup d0 "a" = None
  );
  add_custom_test "DictFun insert & lookup" (fun () ->
    let d0 = DictFun.empty () in
    let d1 = DictFun.insert d0 ("a", 1) in
    DictFun.lookup d1 "a" = Some 1
  );
  add_custom_test "DictFun update existing" (fun () ->
    let d0 = DictFun.empty () in
    let d1 = DictFun.insert d0 ("a", 1) in
    let d2 = DictFun.insert d1 ("a", 2) in
    DictFun.lookup d2 "a" = Some 2
  );
  add_custom_test "DictFun delete existing" (fun () ->
    let d0 = DictFun.empty () in
    let d1 = DictFun.insert d0 ("a", 1) in
    let d2 = DictFun.delete d1 "a" in
    DictFun.lookup d2 "a" = None
  );
  add_custom_test "DictFun delete non-existing" (fun () ->
    let d0 = DictFun.empty () in
    let d1 = DictFun.insert d0 ("a", 1) in
    let d2 = DictFun.delete d1 "b" in
    DictFun.lookup d2 "a" = Some 1 && DictFun.lookup d2 "b" = None
  );
  add_custom_test "DictFun multiple keys" (fun () ->
    let d0 = DictFun.empty () in
    let d1 = DictFun.insert d0 ("a", 1) in
    let d2 = DictFun.insert d1 ("b", 2) in
    DictFun.lookup d2 "a" = Some 1 && DictFun.lookup d2 "b" = Some 2
  )

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
  
  Printf.printf "\n============= CSED-321 HW2 테스트 결과 =============\n";
  
  List.iter (fun tc ->
    let passed = match tc with
      | IntTest (name, f, exp) -> run_test name string_of_int (=) f exp
      | BoolTest (name, f, exp) -> run_test name string_of_bool (=) f exp
      | IntListTest (name, f, exp) -> run_test name string_of_int_list (=) f exp
      | IntListListTest (name, f, exp) -> run_test name string_of_int_list_list (=) f exp
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