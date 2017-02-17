{-# OPTIONS_GHC -Wall #-}

data Light = Green | Yellow | Red | Off deriving Show
data TLights = TLights Light Light Light deriving Show


light2str :: Light -> String
light2str l =
    case l of
      Green -> "G"
      Yellow -> "Y"
      Red -> "R"
      Off -> " "


tLights2str :: TLights -> String
tLights2str tl =
    let TLights l1 l2 l3 = tl in
    "[" ++ (light2str l1) ++ " " ++ (light2str l2) ++ " " ++ (light2str l3) ++ "]"


nextState :: TLights -> TLights
nextState tl =
    case tl of
      TLights Red Off Off -> TLights Red Yellow Off
      TLights Red Yellow Off -> TLights Off Off Green
      TLights Off Off Green -> TLights Off Yellow Off
      TLights Off Yellow Off -> TLights Red Off Off
      _ -> error "invalid state"


test :: Int -> [TLights]
test numSteps =
    snd $ foldr f initialAcc [1..numSteps]
        where initialAcc = (TLights Red Off Off, [])
              f = \_ acc ->
                  let (tl, tls) = acc in
                  (nextState tl, tl : tls)

main :: IO ()
main =
    let states = map tLights2str (test 10) in
    putStrLn $ foldl (\acc str -> str ++ "\n" ++ acc) "" states
