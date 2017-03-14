open Core.Std
open Option.Monad_infix

type 'a t = ('a list * 'a list)

let create (list : 'a list) : 'a t option =
  match list with
  | [] -> None
  | _ -> Some ([], list)


let left (z : 'a t) : 'a t option =
  match z with
  | ([], _) -> None
  | (h :: l, r) -> Some (l, h :: r)


let rec go_left (step : int) (z : 'a t) : 'a t option =
  if step < 0 then None else
    if step = 0 then Some z else
      z |> left >>= go_left (step - 1)


let right (z : 'a t) : 'a t option =
  match z with
  | (_, []) -> None
  | (_, [_last]) -> None
  | (l, h :: r) -> Some (h :: l, r)


let rec go_right (step : int) (z : 'a t) : 'a t option =
  if step < 0 then None else
    if step = 0 then Some z else
      z |> right >>= go_right (step - 1)


let get (z : 'a t) : 'a =
  let (_, r) = z in List.hd_exn r


let set (value : 'a) (z : 'a t) : 'a t =
  let (l, r) = z in (l, value :: List.tl_exn r)


let get_position (z : 'a t) : int =
  let (l, _) = z in List.length l


let rec set_position (pos : int) (z : 'a t) : 'a t option =
  let curr_pos = get_position z in
  if pos = curr_pos then Some z else
    if pos < curr_pos then left z >>= set_position pos else
      right z >>= set_position pos


let to_list (z : 'a t) : 'a list =
  let (left, right) = z in
  (List.rev left) @ right


let to_string ~(f : 'a -> string) (z : 'a t) : string =
  let (left, right) = z in
  let left_s = List.to_string ~f left and
      right_s = List.to_string ~f right in
  "(List_z " ^ left_s ^ " " ^ right_s ^ ")"
