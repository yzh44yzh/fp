{-# OPTIONS_GHC -Wall #-}

import Sokoban
import qualified Parser as P
import qualified Render as R
import qualified System.IO as SIO

drawGame :: State -> String
drawGame state =
    concat ["\n", (R.drawState state), "\n"]


loop :: State -> IO ()
loop state =
    do
      putStr $ drawGame state
      putStr "Move (u)p, (d)own, (l)eft, (r)ight: "
      SIO.hFlush SIO.stdout
      mv <- getLine
      if mv `elem` ["stop", "quit", "q", ""] then return ()
      else mv |> str2move |> doMove state |> loop


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
      game |> P.strToState |> loop
