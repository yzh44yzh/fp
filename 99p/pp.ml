(*
OCaml 99 problems
https://ocaml.org/learn/tutorials/99problems.html
*)

(* 1. Return the last element of a list *)
let last list =
  match List.rev list with
  | [] -> None
  | h :: _ -> Some h


(* 2. Find the last but one (last and penultimate) elements of a list *)
let rec last2 list =
  match list with
  | [] | [_] -> None
  | [i; _] -> Some i
  | _ :: t -> last2 t


(* 3. Find the k'th element of a list *)
let rec nth i list =
  match (i, list) with
  | (1, h :: _) -> Some h
  | (n, _ :: t) when n > 0 -> nth (i - 1) t
  | _ -> None


(* 4. Find the number of elements of a list *)
let rec len2 list acc =
  match list with
  | [] -> acc
  | _ :: t -> len2 t (acc + 1)

let len list =
  len2 list 0


(* 5. Reverse a list *)
let reverse list =
  let rec _reverse list acc =
    match list with
    | [] -> acc
    | h :: t -> _reverse t (h :: acc)
  in
  _reverse list []


(* 6. Find out whether a list is a palindrome *)
let palindrom list =
  list = reverse list


(* 7. Flatten a nested list structure *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec _flatten list acc =
    match list with
    | [] -> acc
    | One item :: t -> _flatten t (item :: acc)
    | Many items :: t -> _flatten t (_flatten items acc)
  in
  reverse (_flatten list [])


(* 8. Eliminate consecutive duplicates of list elements *)

let compress list =
  let rec _compress list acc =
    match list with
    | [] -> reverse acc
    | h :: h2 :: t when h = h2 -> _compress (h2 :: t) acc
    | h :: t -> _compress t (h :: acc)
  in
  _compress list []


(* 9. Pack consecutive duplicates of list elements into sublists *)

let pack list =
  let rec _pack list sublist acc =
    match (list, sublist) with
    | ([], s) -> reverse (s :: acc)
    | (h :: t, hs :: _) when h = hs -> _pack t (h :: sublist) acc
    | (h :: t, _) -> _pack t [h] (sublist :: acc)
  in
  match list with
  | [] -> []
  | h :: t -> _pack t [h] []


(* 10. Run-length encoding of a list *)

let encode list =
  let rec _encode list current acc =
    match (list, current) with
    | ([], c) -> reverse (c :: acc)
    | (h :: t, (num, c)) when h = c -> _encode t (num + 1, c) acc
    | (h :: t, c) -> _encode t (1, h) (c :: acc)
  in
  match list with
  | [] -> []
  | h :: t -> _encode t (1, h) []


(* 11. Modified run-length encoding *)
