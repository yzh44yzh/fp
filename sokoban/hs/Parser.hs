{-# OPTIONS_GHC -Wall #-}

module Parser where

import Sokoban
import qualified Data.Map as M


strToState :: String -> State
strToState str =
    let rows = lines str in
    State { sokFieldWidth = length (head rows)
          , sokFieldHeight = length rows
          , sokField = parseRows rows
          }


parseRows :: [String] -> Field
parseRows rows =
    foldl f M.empty (zip [1,2..] rows)
          where f :: Field -> (Int, String) -> Field
                f acc (rowNum, row) = parseRow rowNum row acc


parseRow :: Int -> String -> Field -> Field
parseRow rowNum row field =
    foldl f field (zip [1,2..] row)
        where f :: Field -> (Int, Char) -> Field
              f acc (colNum, char) = M.insert (rowNum, colNum) (charToCell char) acc


charToCell :: Char -> Cell
charToCell char =
    case char of
      'w' -> Wall
      ' ' -> Free
      't' -> Target
      'p' -> Player
      'b' -> Box
      'B' -> BoxInTarget
      _ -> error "invalid char"
