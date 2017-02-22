{-
Blooming Tree of depth 5 in ASCII graphic:

               |
        _______^_______
       /               \
       |               |
    ___^___         ___^___
   /       \       /       \
   |       |       |       |
  _^_     _^_     _^_     _^_
 /   \   /   \   /   \   /   \
 |   |   |   |   |   |   |   |
 ^   ^   ^   ^   ^   ^   ^   ^
/ \ / \ / \ / \ / \ / \ / \ / \
| | | | | | | | | | | | | | | |
x x x x x x x x x x x x x x x x

-}


drawTree :: Int -> String
drawTree depth
    | depth > 0 = foldl (\acc line -> acc ++ line ++ "\n") "" (drawTree' depth)
    | otherwise = error "invalid depth"


drawTree' :: Int -> [String]
drawTree' depth =
    case depth of
      1 -> ["| ", "x "]
      _ -> let child = drawTree' (depth - 1) in
           (buildHeader depth) ++ (map (\s -> s ++ s) child)


buildHeader :: Int -> [String]
buildHeader depth =
    [concat [s2, "| ", s2]
    ,concat [s40, d4, "^", d4, " ", s40]
    ,concat [s4, "/", s2, "\\", s40]]
    where
      width = round $ 2 ** (fromIntegral depth)
      w2 = width `div` 2
      w4 = width `div` 4
      s2 = str ' ' (w2 - 1)
      s40 = str ' ' w4
      s4 = str ' ' (w4 - 1)
      d4 = str '_' (w4 - 1)


str :: Char -> Int -> String
str ch num =
    take num (repeat ch)


main :: IO ()
main =
    putStrLn $ drawTree 6
