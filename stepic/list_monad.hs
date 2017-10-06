pythagoreanTriple1 :: Int -> [(Int, Int, Int)]
pythagoreanTriple1 x =
    let lst = [1..x] in
    [(a,b,c) | a <- lst, b <- lst, c <- lst, a < b, b < c, c^2 == a^2 + b^2]


pythagoreanTriple2 :: Int -> [(Int, Int, Int)]
pythagoreanTriple2 x = do
    let lst = [1..x]
    a <- lst
    b <- lst
    c <- lst
    True <- return $ a < b
    True <- return $ b < c
    True <- return $ c^2 == a^2 + b^2
    return (a,b,c)


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x =
    let lst = [1..x] in
    lst >>= (\a -> lst >>= (\b -> lst >>= (\c -> fn a b c)))
    where fn :: Int -> Int -> Int -> [(Int, Int, Int)]
          fn a b c = if a < b && b < c && c^2 == a^2 + b^2 then [(a, b, c)] else []
