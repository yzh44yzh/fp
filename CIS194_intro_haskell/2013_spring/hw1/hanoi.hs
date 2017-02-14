{-# OPTIONS_GHC -Wall #-}

type Peg = [Int]
data PegName = A | B | C deriving Show
data Step = Ab | Ac | Ba | Bc | Ca | Cb deriving Show
type Steps = [Step]
data State = State Peg Peg Peg Steps deriving Show


initState :: Int -> State
initState size = State [1..size] [] [] []


solve :: State -> State
solve state =
    let State a0 _ _ _ = state in
    let State a b c steps = move (length a0) A B C state in
    State a b c (reverse steps)


move :: Int -> PegName -> PegName -> PegName -> State -> State
move numDisks fromPeg toPeg tempPeg state =
    case numDisks of
      0 -> state
      1 -> step fromPeg toPeg state
      2 -> (step tempPeg toPeg) .
           (step fromPeg toPeg) .
           (step fromPeg tempPeg) $ state
      _ -> (move (numDisks - 1) tempPeg toPeg fromPeg) .
           (step fromPeg toPeg) .
           (move (numDisks - 1) fromPeg tempPeg toPeg) $ state


step :: PegName -> PegName -> State -> State
step fromPeg toPeg state =
    let State a b c steps = state in
    case (fromPeg, toPeg) of
      (A, B) -> let (f, t) = place a b in State f t c (Ab : steps)
      (A, C) -> let (f, t) = place a c in State f b t (Ac : steps)
      (B, A) -> let (f, t) = place b a in State t f c (Ba : steps)
      (B, C) -> let (f, t) = place b c in State a f t (Bc : steps)
      (C, A) -> let (f, t) = place c a in State t b f (Ca : steps)
      (C, B) -> let (f, t) = place c b in State a t f (Cb : steps)
      _ -> error "invalid step"


place :: Peg -> Peg -> (Peg, Peg)
place fromPeg toPeg =
    case (fromPeg, toPeg) of
      (fhead : ftail, []) -> (ftail, [fhead])
      (fhead : ftail, thead : _) | fhead < thead -> (ftail, fhead : toPeg)
      _ -> error "invalid place"


main :: IO ()
main =
    putStrLn . show . solve . initState $ 8
