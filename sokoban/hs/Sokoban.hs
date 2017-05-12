{-# OPTIONS_GHC -Wall #-}

module Sokoban where

import qualified Data.Map as M

data Cell = Wall | Free | Target deriving Show
type Position = (Int, Int)
type Field = M.Map Position Cell

data Player = Player Position PlayerDirection deriving Show
data Box = Box Position deriving Show

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


str2move :: String -> Move
str2move "u" = MUp
str2move "up" = MUp
str2move "d" = MDown
str2move "down" = MDown
str2move "l" = MLeft
str2move "left" = MLeft
str2move "r" = MRight
str2move "right" = MRight
str2move _ = MNone


doMove :: State -> Move -> State
doMove state mv =
    state { sokPlayer = player3, sokBoxes = boxes2 }
        where
          State { sokField = field, sokPlayer = player, sokBoxes = boxes } = state
          player2 = movePlayer mv player
          Player pos _ = player2
          player3 = if playerAllowed field pos then player2 else player
          boxes2 = moveBoxes mv pos boxes


playerAllowed :: Field -> Position -> Bool
playerAllowed field (row, col) =
    case (M.!) field (row, col) of
      Wall -> False
      Free -> True
      Target -> True


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


moveBoxes :: Move -> Position -> [Box] -> [Box]
moveBoxes mv pPos boxes =
    map f boxes
        where f :: Box -> Box
              f box@(Box bPos)
                | pPos == bPos = Box (move mv bPos)
                | otherwise = box
