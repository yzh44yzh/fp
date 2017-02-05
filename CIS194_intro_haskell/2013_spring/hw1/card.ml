let validate (cardNum : int) : bool =
  cardNum |> num2digits |> doubleOddPositions |> sumDigits
  |> fun x -> x mod 10 == 0


let num2digits (num : int) : int list =
  let rec _num2digits (n : int) (acc : int list) : int list =
    if n < 10 then n :: acc
    else
      let d = n / 10 in
      let r = n mod 10 in
      _num2digits d (r :: acc)
  in
  _num2digits num []


let doubleOddPositions (nums : int list) : int list =
  let f (acc : bool * int list) (num : int) : (bool * int list) =
    match acc with
    | (true, a) -> (false, (num * 2) :: a)
    | (false, a) -> (true, num :: a)
  in
  List.fold_left f (true, []) nums
  |> snd
  |> List.rev


let rec sumDigits (digits : int list) : int =
  let f (a : int) (d : int) : int =
    a + if d < 10 then d
        else d |> num2digits |> sumDigits
  in
  List.fold_left f 0 digits
