{-# OPTIONS_GHC -Wall #-}

module Sokoban where

import qualified Data.Map as M

data Cell = Wall | Free | Target deriving Show
type Field = M.Map (Int, Int) Cell

data Player = Player Int Int PlayerDirection deriving Show
data Box = Box Int Int deriving Show

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
                 , sokPlayer = Player 1 1 PLeft
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
doMove state move =
    state { sokPlayer = player2 }
        where
          State { sokPlayer = player } = state
          player2 = movePlayer move player


movePlayer :: Move -> Player -> Player
movePlayer MUp (Player row col d) = Player (row - 1) col d
movePlayer MDown (Player row col d) = Player (row + 1) col d
movePlayer MLeft (Player row col _) = Player row (col - 1) PLeft
movePlayer MRight (Player row col _) = Player row (col + 1) PRight
movePlayer MNone player = player
