open Core.Std
open Option.Monad_infix

module Z = List_z

type 'a t = ('a Z.t Z.t)


let create (list : 'a list list) : 'a t option =
  let rec create_rows (rows : 'a list list) (acc : 'a Z.t list) : 'a Z.t list option =
    match rows with
    | [] -> Some (List.rev acc)
    | row :: rest ->
       match Z.create row with
       | None -> None
       | Some lz -> create_rows rest (lz :: acc)
  in
  match list with
  | [] -> None
  | _ -> create_rows list [] >>= Z.create


let to_matrix (mz : 'a t) : 'a list list =
  mz
  |> Z.to_list
  |> List.map ~f:Z.to_list


let to_string ~(f : 'a -> string) (mz : 'a t) : string =
  let folder (acc : string) (lz : 'a Z.t) : string =
    acc ^ " | " ^ (Z.to_string ~f:f lz)
  in
  mz
  |> Z.to_list
  |> List.fold ~init:"<Matrix_z" ~f:folder
  |> fun s -> s ^ ">"
