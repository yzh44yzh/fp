open Core.Std
open Option.Monad_infix
open OUnit2

module Z = List_z

(* Util functions *)

let compare (list : 'a list) (zipper : 'a Z.t) (printer : 'a list -> string) : 'a Z.t =
  assert_equal list (Z.to_list zipper) ~printer:printer;
  zipper


let compare_i (list : int list) (zipper : int Z.t) : int Z.t =
  compare list zipper (List.to_string ~f:Int.to_string)


let compare_f (list : float list) (zipper : float Z.t) : float Z.t =
  compare list zipper (List.to_string ~f:Float.to_string)


let compare_s (list : string list) (zipper : string Z.t) : string Z.t =
  compare list zipper (List.to_string ~f:String.to_string)


let check_pos_value (pos : int) (value : string) (zipper : string Z.t) : string Z.t =
  assert_equal pos (Z.get_position zipper) ~printer:Int.to_string;
  assert_equal value (Z.get zipper) ~printer:String.to_string;
  zipper


(* Tests *)

let create_test _state =
  Z.create [1; 2; 3] >>| compare_i [1; 2; 3] |> ignore;
  Z.create [0.1; 0.2; 0.3; 0.4] >>| compare_f [0.1; 0.2; 0.3; 0.4] |> ignore;
  Z.create ["aa"; "bb"; "cc"; "dd"] >>| compare_s ["aa"; "bb"; "cc"; "dd"] |> ignore;
  assert_equal None (Z.create [])


let left_test _state =
  Z.create ["a"; "b"; "c"; "d"]
  >>| check_pos_value 0 "a"
  >>= Z.go_right 3
  >>| check_pos_value 3 "d"
  >>= Z.left
  >>| check_pos_value 2 "c"
  >>= Z.left
  >>| check_pos_value 1 "b"
  >>= Z.left
  >>| check_pos_value 0 "a"
  >>= Z.left
  |> assert_equal None


let right_test _state =
  Z.create ["a"; "b"; "c"; "d"]
  >>| check_pos_value 0 "a"
  >>= Z.right
  >>| check_pos_value 1 "b"
  >>= Z.right
  >>| check_pos_value 2 "c"
  >>= Z.right
  >>| check_pos_value 3 "d"
  >>= Z.right
  |> assert_equal None


let go_left_test _state =
  let z = Z.create ["a"; "b"; "c"; "d"] >>= Z.go_right 3 in
  z >>= Z.go_left 0 >>| check_pos_value 3 "d" |> ignore;
  z >>= Z.go_left 1 >>| check_pos_value 2 "c" |> ignore;
  z >>= Z.go_left 2 >>| check_pos_value 1 "b" |> ignore;
  z >>= Z.go_left 3 >>| check_pos_value 0 "a" |> ignore;
  assert_equal z (z >>= Z.go_left 0);
  assert_equal None (z >>= Z.go_left 4);
  assert_equal None (z >>= Z.go_left (-1))


let go_right_test _suite =
  let z = Z.create ["a"; "b"; "c"; "d"] in
  z >>= Z.go_right 0 >>| check_pos_value 0 "a" |> ignore;
  z >>= Z.go_right 1 >>| check_pos_value 1 "b" |> ignore;
  z >>= Z.go_right 2 >>| check_pos_value 2 "c" |> ignore;
  z >>= Z.go_right 3 >>| check_pos_value 3 "d" |> ignore;
  assert_equal z (z >>= Z.go_right 0);
  assert_equal None (z >>= Z.go_right 4);
  assert_equal None (z >>= Z.go_right (-1))


let set_test _state =
  Z.create [1; 2; 3; 4; 5]
  >>| Z.set 10
  >>| compare_i [10; 2; 3; 4; 5]
  >>= Z.right >>| Z.set 20
  >>| compare_i [10; 20; 3; 4; 5]
  >>= Z.right >>| Z.set 30
  >>| compare_i [10; 20; 30; 4; 5]
  >>= Z.right >>| Z.set 40
  >>| compare_i [10; 20; 30; 40; 5]
  >>= Z.right >>| Z.set 50
  >>| compare_i [10; 20; 30; 40; 50]
  |> ignore


let set_position_test _state =
  let z = Z.create ["a"; "b"; "c"; "d"; "e"] in
  z >>= Z.set_position 0 >>| check_pos_value 0 "a" |> ignore;
  z >>= Z.set_position 1 >>| check_pos_value 1 "b" |> ignore;
  z >>= Z.set_position 2 >>| check_pos_value 2 "c" |> ignore;
  z >>= Z.set_position 3 >>| check_pos_value 3 "d" |> ignore;
  z >>= Z.set_position 4 >>| check_pos_value 4 "e" |> ignore


let to_list_test _state =
  let cmp l1 l2 = assert_equal l1 l2 ~printer:(List.to_string ~f:Int.to_string) in
  Z.create [1; 2; 3] >>| Z.to_list >>| cmp [1; 2; 3] |> ignore;
  Z.create [4; 5; 6] >>= Z.right >>| Z.to_list >>| cmp [4; 5; 6] |> ignore;
  Z.create [4; 5; 6; 7; 8] >>= Z.right >>= Z.right >>| Z.to_list >>| cmp [4; 5; 6; 7; 8] |> ignore;
  Z.create [22; 33; 44] >>= Z.set_position 2 >>| Z.to_list >>| cmp [22; 33; 44] |> ignore


let to_string_test _state =
  let cmp str zipper = assert_equal str (Z.to_string zipper ~f:Int.to_string) ~printer:String.to_string in
  Z.create [1; 2; 3] >>| cmp "(List_z () (1 2 3))" |> ignore;
  Z.create [4; 5; 6] >>= Z.right >>| cmp "(List_z (4) (5 6))" |> ignore;
  Z.create [4; 5; 6; 7; 8] >>= Z.right >>= Z.right >>| cmp "(List_z (5 4) (6 7 8))" |> ignore;
  Z.create [22; 33; 44] >>= Z.set_position 2 >>| cmp "(List_z (33 22) (44))" |> ignore


let suite =
  "list_z_suite" >:::
    [
      "create_test" >:: create_test;
      "left_test" >:: left_test;
      "right_test" >:: right_test;
      "go_left_test" >:: go_left_test;
      "go_right_test" >:: go_right_test;
      "set_test" >:: set_test;
      "set_position_test" >:: set_position_test;
      "to_list_test" >:: to_list_test;
      "to_string_test" >:: to_string_test
    ]
