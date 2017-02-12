type peg = int list
type pegName = A | B | C
type step = Ab | Ac | Ba | Bc | Ca | Cb
type steps = step list
type state = State of (peg * peg * peg * steps)
exception Invalid_step of (pegName * pegName)
exception Invalid_place of (peg * peg)


let init (size : int) : state =
  let rec _seq (next : int) : int list =
    if next == size then [next]
    else next :: _seq (next + 1)
  in
  if size < 1 then State ([], [], [], [])
  else State (seq size, [], [], [])


let place (fromPeg : peg) (toPeg : peg) : (peg * peg) =
  match (fromPeg, toPeg) with
  | (fhead :: ftail, []) -> (ftail, [fhead])
  | (fhead :: ftail, thead :: _) when fhead < thead -> (ftail, fhead :: toPeg)
  | _ -> raise (Invalid_place (fromPeg, toPeg))


let step (fromPeg : pegName) (toPeg : pegName) (state : state) : state =
  let State (a, b, c, steps) = state in
  match (fromPeg, toPeg) with
  | (A, B) -> let (f, t) = place a b in State (f, t, c, Ab :: steps)
  | (A, C) -> let (f, t) = place a c in State (f, b, t, Ac :: steps)
  | (B, A) -> let (f, t) = place b a in State (t, f, c, Ba :: steps)
  | (B, C) -> let (f, t) = place b c in State (a, f, t, Bc :: steps)
  | (C, A) -> let (f, t) = place c a in State (t, b, f, Ca :: steps)
  | (C, B) -> let (f, t) = place c b in State (a, t, f, Cb :: steps)
  | _ -> raise (Invalid_step (fromPeg, toPeg))


let rec move (numDisks : int) (fromPeg : pegName) (toPeg : pegName) (tempPeg : pegName) (state : state) : state =
  match numDisks with
  | 0 -> state
  | 1 -> step fromPeg toPeg state
  | 2 -> state |> step fromPeg tempPeg |> step fromPeg toPeg |> step tempPeg toPeg
  | _ -> state
         |> move (numDisks - 1) fromPeg tempPeg toPeg
         |> step fromPeg toPeg
         |> move (numDisks - 1) tempPeg toPeg fromPeg


let solve (state : state) : state =
  let State (a0, _, _, _) = state in
  let State (a, b, c, steps) = move (List.length a0) A B C state in
  State (a, b, c, List.rev steps)
