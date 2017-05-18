{-# OPTIONS_GHC -Wall #-}

module Sokoban where

import qualified Data.Map as M

data Cell = Wall | Free | Target deriving (Show, Eq)
type Position = (Int, Int)
type Field = M.Map Position Cell

data Player = Player Position PlayerDirection deriving Show
data Box = Box Position deriving (Show, Eq)

data Move = MUp | MDown | MLeft | MRight | MNone deriving Show
data PlayerDirection = PLeft | PRight deriving Show

data State = State { sokFieldWidth :: Int
                   , sokFieldHeight :: Int
                   , sokField :: Field
                   , sokPlayer :: Player
                   , sokBoxes :: [Box]
                   } deriving Show


newState :: State
newState = State { sokFieldWidth = 1
                 , sokFieldHeight = 1
                 , sokField = M.empty
                 , sokPlayer = Player (1, 1) PLeft
                 , sokBoxes = []
                 }


(|>) :: a -> (a -> b) -> b
(|>) a f = f a


doMove :: State -> Move -> State
doMove state@State{sokPlayer = player, sokBoxes = boxes} mv =
    if (noWallAt state newPos) && okWithBoxMove
    then state { sokPlayer = player2, sokBoxes = boxes2 }
    else state
        where
          Player currPos _ = player
          newPos = move mv currPos
          player2 = movePlayer mv player
          boxes2 = moveBoxes mv newPos state boxes
          boxShouldMoved = hasBoxAt state newPos
          boxWasMoved = boxes /= boxes2
          okWithBoxMove = (not boxShouldMoved) || (boxShouldMoved && boxWasMoved)


noWallAt :: State -> Position -> Bool
noWallAt State{sokField = field} (row, col) =
    case (M.!) field (row, col) of
      Wall -> False
      Free -> True
      Target -> True


noBoxAt :: State -> Position -> Bool
noBoxAt State{sokBoxes = boxes} pos =
    foldl f True boxes
          where f :: Bool -> Box -> Bool
                f False _ = False
                f _ (Box bPos) = bPos /= pos


hasBoxAt :: State -> Position -> Bool
hasBoxAt state pos =
    not (noBoxAt state pos)


move :: Move -> Position -> Position
move MUp (row, col) = ((row - 1), col)
move MDown (row, col) = ((row + 1), col)
move MLeft (row, col) = (row, (col - 1))
move MRight (row, col) = (row, (col + 1))
move MNone p = p


movePlayer :: Move -> Player -> Player
movePlayer MLeft (Player p _) = Player (move MLeft p) PLeft
movePlayer MRight (Player p _) = Player (move MRight p) PRight
movePlayer mv (Player p d) = Player (move mv p) d


moveBoxes :: Move -> Position -> State -> [Box] -> [Box]
moveBoxes mv atPos state boxes =
    map (\box -> moveBox mv atPos state box) boxes


moveBox :: Move -> Position -> State -> Box -> Box
moveBox mv atPos state box@(Box pos) =
    if pos == atPos && (noWallAt state newPos) && (noBoxAt state newPos)
    then Box newPos
    else box
        where newPos = move mv pos


win :: State -> Bool
win State{sokField = field, sokBoxes = boxes} =
    map (\(Box pos) -> (M.!) field pos) boxes |> all (==Target)
