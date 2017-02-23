{-# OPTIONS_GHC -Wall #-}

data Cell = Wall | Free | Target | Player | Box | BoxInTarget
type State = [[Cell]]


strToState :: String -> State
strToState str =
    map (\line -> map charToCell line) (lines str)
        where charToCell =
                  \char ->
                      case char of
                        'w' -> Wall
                        ' ' -> Free
                        't' -> Target
                        'p' -> Player
                        'b' -> Box
                        'B' -> BoxInTarget
                        _ -> error "invalid str"


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


escape :: String
escape = "\x001b"


startColor :: String -> String
startColor color =
    escape ++ "[" ++ color ++ "m"


endColor :: String
endColor = escape ++ "[0m"


drawState :: State -> String
drawState state =
    let drawCell = \cell -> (startColor $ colorForCell cell) ++ (showCell cell) in
    let drawLine = \row -> "    " ++ (concat $ map drawCell row) ++ endColor ++ "\n" in
    concat $ map drawLine state


drawGame :: State -> String
drawGame state =
    concat ["\n", (drawState state), "\n"]


main :: IO ()
main =
    let game = "          \n" ++
               "   wwwww  \n" ++
               " www   w  \n" ++
               " wtpb  w  \n" ++
               " www btw  \n" ++
               " wtwwb w  \n" ++
               " w w t ww \n" ++
               " wb bbbtw \n" ++
               " w   t  w \n" ++
               " wwwwwwww \n" ++
               "          \n"
    in
    (putStr . drawGame . strToState) game
