{-# OPTIONS_GHC -Wall #-}

module Sokoban where

import qualified Data.Map as M

data Cell = Wall | Free | Target | Player | Box | BoxInTarget deriving Show

type Field = M.Map (Int, Int) Cell

data State = State { sokFieldWidth :: Int
                   , sokFieldHeight :: Int
                   , sokField :: Field
                   } deriving Show

data Direction = DUp | DDown | DLeft | DRight deriving Show

data Position = Pos Int Int deriving Show


(|>) :: a -> (a -> b) -> b
(|>) a f = f a


move :: Direction -> Position -> Position
move DUp (Pos x y) = Pos x (y - 1)
move DDown (Pos x y) = Pos x (y + 1)
move DLeft (Pos x y) = Pos (x - 1) y
move DRight (Pos x y) = Pos (x + 1) y
