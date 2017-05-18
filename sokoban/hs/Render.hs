{-# OPTIONS_GHC -Wall #-}

module Render where

import Sokoban
import qualified Data.Map as M


drawState :: State -> String
drawState state =
    map f [1..height] |> concat
    where
      State {sokFieldHeight = height} = state
      f :: Int -> String
      f rowNum = drawRow rowNum state


drawRow :: Int -> State -> String
drawRow rowNum state =
    map f [1..width] |> concat |> (++ endColor ++ "\n")
    where
      State {sokFieldWidth = width, sokField = field, sokPlayer = player} = state
      getCell :: Int -> Cell
      getCell colNum = (M.!) field (rowNum, colNum)
      f :: Int -> String
      f colNum
        | hasPlayer state rowNum colNum = drawPlayer player
        | hasBox state rowNum colNum = getCell colNum |> drawBox
        | otherwise = getCell colNum |> drawCell


hasPlayer :: State -> Int -> Int -> Bool
hasPlayer state rowNum colNum =
    pRowNum == rowNum && pColNum == colNum
        where
          State { sokPlayer = player } = state
          Player (pRowNum, pColNum) _ = player


hasBox :: State -> Int -> Int -> Bool
hasBox state rowNum colNum =
    any f boxes
    where
      State { sokBoxes = boxes } = state
      f :: Box -> Bool
      f (Box (bRowNum, bColNum)) =
          bRowNum == rowNum && bColNum == colNum


draw :: String -> String -> String
draw color cellStr =
    concat [startColor color, cellStr]


drawPlayer :: Player -> String
drawPlayer player = draw playerColor (playerStr player)


drawBox :: Cell -> String
drawBox Target = draw boxInTargetColor boxStr
drawBox _ = draw boxColor boxStr


drawCell :: Cell -> String
drawCell Wall = draw wallCellColor wallCellStr
drawCell Free = draw freeCellColor freeCellStr
drawCell Target = draw targetCellColor targetCellStr


wallCellColor :: String
wallCellColor = "0;32;40" -- black on black

freeCellColor :: String
freeCellColor = "0;37;43" -- white on orange

targetCellColor :: String
targetCellColor = "0;37;43" -- yellow on orange

playerColor :: String
playerColor = "1;31;43" -- red on orange

boxColor :: String
boxColor = "1;33;43" -- yellow on orange

boxInTargetColor :: String
boxInTargetColor = "1;32;43" -- green on orange

wallCellStr :: String
wallCellStr = "  "

freeCellStr :: String
freeCellStr = "  "

targetCellStr :: String
targetCellStr = ".."

playerStr :: Player -> String
playerStr (Player _ PLeft) = "@^"
playerStr (Player _ PRight) = "^@"

boxStr :: String
boxStr = "[]"


escape :: String
escape = "\x001b"


startColor :: String -> String
startColor color =
    concat [escape, "[", color, "m"]


endColor :: String
endColor =
    concat [escape, "[0m"]
