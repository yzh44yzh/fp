open OUnit2 (* http://ounit.forge.ocamlcore.org/api-ounit/index.html *)


let all_suites =
  "all_suites" >:::
    [
      List_z_test.suite;
      Matrix_z_test.suite
    ]


let () =
  run_test_tt_main all_suites
