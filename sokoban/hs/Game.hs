{-# OPTIONS_GHC -Wall #-}

import Sokoban
import Render


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
