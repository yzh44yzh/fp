{-# OPTIONS_GHC -Wall #-}

module Golf where

import Test.HUnit


skips :: [a] -> [[a]]
skips list =
    [every_nth list index | index <- [1..len]]
        where len = length list


every_nth :: [a] -> Int -> [a]
every_nth list num =
    map fst . filter f $ zip list [1..]
        where f (_, index) = index `rem` num == 0


localMaxima :: (Ord a) => [a] -> [a]
localMaxima list = localMaxima' list []


localMaxima' :: (Ord a) => [a] -> [a] -> [a]
localMaxima' list acc =
    case list of
      v1:v2:v3:rest | v1 < v2 && v2 > v3 -> localMaxima' (v3:rest) (v2:acc)
      _:v2:v3:rest -> localMaxima' (v2:v3:rest) acc
      _ -> reverse acc


--histogram :: [Int] -> String
histogram :: [Int] -> [String]
histogram list =
    draw_hist' $ foldl count' [0,0,0,0,0,0,0,0,0,0] list


count' :: [Int] -> Int -> [Int]
count' acc index =
    update_count' acc index []


update_count' :: [Int] -> Int -> [Int] -> [Int]
update_count' [] _ acc = reverse acc
update_count' (h:t) 0 acc =
    (reverse acc) ++ ((h + 1) : t)
update_count' (h:t) index acc =
    update_count' t (index-1) (h:acc)


draw_hist' :: [Int] -> [String]
draw_hist' list =
    map f (zip list [0..])
        where f :: (Int, Int) -> String
              f (val, index) = (show (index :: Int)) ++ "=" ++ (s val)
              s :: Int -> String
              s val = take val $ repeat '*'


test_hist :: [String]
test_hist = histogram [0,4,8,1,2,0,3,2,0,5,0]
-- ["0=****","1=*","2=**","3=*","4=*","5=*","6=","7=","8=*","9="]
-- TODO:
-- fill with spaces to equal length (need max length)
-- rotate matrix
-- join with \n


-- runTestTT tests
-- https://github.com/hspec/HUnit
tests :: Test
tests = TestList ["skips 1" ~: ["ABCD", "BD", "C", "D"] ~=? skips "ABCD"
                 ,"skips 2" ~: ["hello!", "el!", "l!", "l", "o", "!"] ~=? skips "hello!"
                 ,"skips 3" ~: ([[1]]::[[Int]]) ~=? skips [1]
                 ,"skips 4" ~: [[True, False], [False]] ~=? skips [True, False]
                 ,"skips 5" ~: ([]::[[Int]]) ~=? skips []
                 ,"localMaxima 1" ~: ([9, 6]::[Int]) ~=? localMaxima [2, 9, 5, 6, 1]
                 ,"localMaxima 2" ~: ([4]::[Int])    ~=? localMaxima [2, 3, 4, 1, 5]
                 ,"localMaxima 3" ~: ([]::[Int])     ~=? localMaxima [1, 2, 3, 4, 5]
                 ,"localMaxima 4" ~: ([9]::[Int])    ~=? localMaxima [2, 9, 5]
                 ,"localMaxima 5" ~: ([]::[Int])     ~=? localMaxima [2, 9]
                 ,"localMaxima 6" ~: ([]::[Int])     ~=? localMaxima [9]
                 ,"localMaxima 7" ~: ([]::[Int])     ~=? localMaxima []
                 ]
