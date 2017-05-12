{-# OPTIONS_GHC -Wall #-}

module Parser where

import Sokoban
import qualified Data.Map as M


strToState :: String -> State
strToState str =
    let rows = lines str in
    parseRows rows (newState { sokFieldWidth = length (head rows)
                             , sokFieldHeight = length rows})


parseRows :: [String] -> State -> State
parseRows rows state =
    foldl f state (zip [1..] rows)
        where f :: State -> (Int, String) -> State
              f acc (rowNum, row) = parseRow rowNum row acc


parseRow :: Int -> String -> State -> State
parseRow rowNum row state =
    foldl f state (zip [1..] row)
        where f :: State -> (Int, Char) -> State
              f acc (colNum, char) =
                  let State { sokField = field, sokBoxes = boxes } = acc in
                  let addCell cell = M.insert (rowNum, colNum) cell field in
                  case char of
                    'p' -> acc { sokField = addCell Free
                               , sokPlayer = (Player (rowNum, colNum) PLeft)
                               }
                    'b' -> acc { sokField = addCell Free
                               , sokBoxes = (Box (rowNum, colNum)) : boxes
                               }
                    'w' -> acc { sokField = addCell Wall }
                    't' -> acc { sokField = addCell Target }
                    ' ' -> acc { sokField = addCell Free }
                    _ -> error "invalid char"
