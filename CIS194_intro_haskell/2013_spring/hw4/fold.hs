{-# OPTIONS_GHC -Wall #-}

(|>) :: a -> (a -> b) -> b
(|>) a f = f a


-- Exercise 1.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' lst = lst |> filter even |> map ((-)2) |> product



fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


-- fun2' -- no idea

-- Exercise 2.

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree _ = Leaf
-- TODO:
-- insert value to Tree
-- balance tree
-- update depths
-- Too much useless boring work.


-- Exercise 3.

xor :: [Bool] -> Bool
xor list = list |> filter id |> length |> odd

map' :: (a -> b) -> [a] -> [b]
map' f lst = foldr (\v acc -> (f v) : acc) [] lst

-- 3. Implement foldl using foldr.
-- Yet another useless exercise


-- Exercise 4.
