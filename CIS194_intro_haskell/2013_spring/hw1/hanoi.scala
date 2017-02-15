type Peg = List[Int]

sealed abstract class PegName
case object A extends PegName
case object B extends PegName
case object C extends PegName

sealed abstract class Step
case object Ab extends Step
case object Ac extends Step
case object Ba extends Step
case object Bc extends Step
case object Ca extends Step
case object Cb extends Step

type Steps = List[Step]

case class State(a: Peg, b: Peg, c: Peg, steps: Steps)

object Hanoi {

  def init(size: Int): State =
    new State((1 to size).toList, Nil, Nil, Nil)


  def solve(state: State): State = {
    val res = move(state.a.length, A, B, C, state)
    new State(res.a, res.b, res.c, res.steps.reverse)
  }


  def move(numDisks: Int, fromPeg: PegName, toPeg: PegName, tempPeg: PegName, state: State): State =
    numDisks match {
      case 0 => state
      case 1 => step(fromPeg, toPeg, state)
      case 2 =>
        step(tempPeg, toPeg,
          step(fromPeg, toPeg,
            step(fromPeg, tempPeg, state)))
      case _ =>
        val n = numDisks - 1
        move(n, tempPeg, toPeg, fromPeg,
          step(fromPeg, toPeg,
            move(n, fromPeg, tempPeg, toPeg, state)))
    }


  def step(fromPeg: PegName, toPeg: PegName, state: State): State =
    (fromPeg, toPeg) match {
      case (A, B) => val (f, t) = place(state.a, state.b); new State(f, t, state.c, Ab :: state.steps)
      case (A, C) => val (f, t) = place(state.a, state.c); new State(f, state.b, t, Ac :: state.steps)
      case (B, A) => val (f, t) = place(state.b, state.a); new State(t, f, state.c, Ba :: state.steps)
      case (B, C) => val (f, t) = place(state.b, state.c); new State(state.a, f, t, Bc :: state.steps)
      case (C, A) => val (f, t) = place(state.c, state.a); new State(t, state.b, f, Ca :: state.steps)
      case (C, B) => val (f, t) = place(state.c, state.b); new State(state.a, t, f, Cb :: state.steps)
      case _ => throw new IllegalArgumentException()
    }


  def place(fromPeg: Peg, toPeg: Peg): (Peg, Peg) =
    (fromPeg, toPeg) match {
      case (fhead :: ftail, Nil) => (ftail, List(fhead))
      case (fhead :: ftail, thead :: _) if fhead < thead => (ftail, fhead :: toPeg)
      case _ => throw new IllegalArgumentException()
    }
}
