open OUnit2
open CpecmuCompiler.A3_typechecking

let tests = "test suite for sample typechecking" >::: [
  "2+3" >:: (fun _ -> assert_equal true (typecheck_string "2+3"));
  "true && false" >:: (fun _ -> assert_equal true (typecheck_string "true && false"));
  "1+true" >:: (fun _ -> assert_equal false (typecheck_string "1+true"));
  "1 || true" >:: (fun _ -> assert_equal false (typecheck_string "1 || true"));

  "2+3" >:: (fun _ -> assert_equal true (typecheck_string "2+3"));
  "2*3" >:: (fun _ -> assert_equal true (typecheck_string "2*3"));
  "true || false" >:: (fun _ -> assert_equal true (typecheck_string "true || false"));
  "true && false" >:: (fun _ -> assert_equal true (typecheck_string "true && false"));

  "1+true" >:: (fun _ -> assert_equal false (typecheck_string "1+true"));
  "1*true" >:: (fun _ -> assert_equal false (typecheck_string "1*true"));
  "1 || true" >:: (fun _ -> assert_equal false (typecheck_string "1 || true"));
  "1 && true" >:: (fun _ -> assert_equal false (typecheck_string "1 && true"));
]

let _ = run_test_tt_main tests
