{-# OPTIONS_GHC -Wall #-}

import Sokoban
import qualified Parser as P
import qualified Render as R


drawGame :: State -> String
drawGame state =
    concat ["\n", (R.drawState state), "\n"]


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
      game |> P.strToState |> drawGame |> putStr
