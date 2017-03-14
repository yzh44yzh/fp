open Core.Std
open Option.Monad_infix
open OUnit2


let create_test _state =
  assert_equal [1; 2; 3] [1; 2; 3]


let suite =
  "matrix_z_suite" >:::
    [
      "create_test" >:: create_test
    ]
