open Core.Std
open Option.Monad_infix

module LZ = List_z
module MZ = Matrix_z


let () =
  LZ.create [1; 2; 3; 4]
  >>| LZ.to_string ~f:Int.to_string
  >>| printf "%s\n"
  |> ignore;
  MZ.create [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]
  >>| MZ.to_string ~f:Int.to_string
  >>| printf "%s\n"
  |> ignore;
  printf "Done\n"
