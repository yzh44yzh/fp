{-# OPTIONS_GHC -Wall #-}

import Sokoban
import qualified Parser as P
import qualified Render as R
import qualified System.IO as SIO
import qualified System.Process as SP

loop :: State -> IO ()
loop state =
    do
      _ <- SP.system "clear"
      putStr $ drawGame state
      putStr "Move with (wasd) keys or (q)iut: "
      mv <- getChar
      if mv == 'q' then return ()
      else mv |> P.char2move |> doMove state |> loop


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
    do
      SIO.hSetBuffering SIO.stdin SIO.NoBuffering
      SIO.hSetBuffering SIO.stdout SIO.NoBuffering
      game |> P.strToState |> loop
