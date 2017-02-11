type peg = int list
type pegName = A | B | C
type step = Ab | Ac | Ba | Bc | Ca | Cb
type steps = step list
type state = State of (peg * peg * peg * steps)
exception Invalid_step of (pegName * pegName)
exception Invalid_place of (peg * peg)


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


let place (fromPeg : peg) (toPeg : peg) : (peg * peg) =
  match (fromPeg, toPeg) with
  | (fhead :: ftail, []) -> (ftail, [fhead])
  | (fhead :: ftail, thead :: _) when fhead < thead -> (ftail, fhead :: toPeg)
  | _ -> raise (Invalid_place (fromPeg, toPeg))
