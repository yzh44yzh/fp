{-# OPTIONS_GHC -Wall #-}

module Render where

import Sokoban

escape :: String
escape = "\x001b"


startColor :: String -> String
startColor color =
    escape ++ "[" ++ color ++ "m"


endColor :: String
endColor = escape ++ "[0m"



showCell :: Cell -> String
showCell cell =
    case cell of
      Wall -> "  "
      Free -> "  "
      Target -> ".."
      Player -> "@^"
      Box -> "[]"
      BoxInTarget -> "[]"


colorForCell :: Cell -> String
colorForCell cell =
    case cell of
      Wall   -> "0;32;40" -- black on black
      Free   -> "0;37;43" -- white on orange
      Target -> "0;37;43" -- yellow on orange
      Player -> "1;31;43" -- red on orange
      Box    -> "1;33;43" -- yellow on orange
      BoxInTarget -> "1;32;43" -- green on orange


drawState :: State -> String
drawState state =
    concat $ map drawLine state
    where
      drawCell cell = (startColor $ colorForCell cell) ++ (showCell cell)
      drawLine row = "    " ++ (concat $ map drawCell row) ++ endColor ++ "\n"
