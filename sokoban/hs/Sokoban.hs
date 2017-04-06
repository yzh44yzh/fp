{-# OPTIONS_GHC -Wall #-}

module Sokoban where

data Cell = Wall | Free | Target | Player | Box | BoxInTarget
type State = [[Cell]]
