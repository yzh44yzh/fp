{-# OPTIONS_GHC -Wall #-}

module Sokoban where

import qualified Data.Map as M

data Cell = Wall | Free | Target deriving Show
type Field = M.Map (Int, Int) Cell

-- data Direction = DUp | DDown | DLeft | DRight deriving Show
-- data Position = Pos Int Int deriving Show
data Player = Player Int Int deriving Show
data Box = Box Int Int deriving Show

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
                 , sokPlayer = Player 1 1
                 , sokBoxes = []
                 }


(|>) :: a -> (a -> b) -> b
(|>) a f = f a


-- move :: Direction -> Position -> Position
-- move DUp (Pos x y) = Pos x (y - 1)
-- move DDown (Pos x y) = Pos x (y + 1)
-- move DLeft (Pos x y) = Pos (x - 1) y
-- move DRight (Pos x y) = Pos (x + 1) y
