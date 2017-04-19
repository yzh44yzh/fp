{-# OPTIONS_GHC -Wall #-}

module Golf where

import Test.HUnit


(|>) :: a -> (a -> b) -> b
(|>) a f = f a


skips :: [a] -> [[a]]
skips list =
    [every_nth list index | index <- [1..len]]
        where len = length list


every_nth :: [a] -> Int -> [a]
every_nth list num =
    map fst . filter f $ zip list [1..]
        where f (_, index) = index `rem` num == 0


local_maxima :: (Ord a) => [a] -> [a]
local_maxima list = local_maxima' list []


local_maxima' :: (Ord a) => [a] -> [a] -> [a]
local_maxima' list acc =
    case list of
      v1:v2:v3:rest | v1 < v2 && v2 > v3 -> local_maxima' (v3:rest) (v2:acc)
      _:v2:v3:rest -> local_maxima' (v2:v3:rest) acc
      _ -> reverse acc


histogram :: [Int] -> String
histogram list =
    list |> hist_data |> draw_hist


hist_data :: [Int] -> [Int]
hist_data list =
    foldl count [0,0,0,0,0,0,0,0,0,0] list
        where count :: [Int] -> Int -> [Int]
              count acc index = update_count' acc index []
              update_count' :: [Int] -> Int -> [Int] -> [Int]
              update_count' [] _ acc = reverse acc
              update_count' (h:t) 0 acc =
                  (reverse acc) ++ ((h + 1) : t)
              update_count' (h:t) index acc =
                  update_count' t (index-1) (h:acc)


draw_hist :: [Int] -> String
draw_hist list =
    zip list [0..]
        |> map show_val
        |> rotate
        |> reverse
        |> unlines
        where mx = maximum list
              show_val :: (Int, Int) -> String
              show_val (val, index) = (show (index :: Int)) ++ "=" ++ (stars val) ++ (spaces val)
              stars :: Int -> String
              stars val = take val $ repeat '*'
              spaces :: Int -> String
              spaces val = take (mx - val) $ repeat ' '


rotate :: [[a]] -> [[a]]
rotate list =
    case head list of
      [] -> []
      _ -> (map head list) : (rotate $ map tail list)



-- runTestTT tests
-- https://github.com/hspec/HUnit
tests :: Test
tests = TestList ["skips 1" ~: ["ABCD", "BD", "C", "D"] ~=? skips "ABCD"
                 ,"skips 2" ~: ["hello!", "el!", "l!", "l", "o", "!"] ~=? skips "hello!"
                 ,"skips 3" ~: ([[1]]::[[Int]]) ~=? skips [1]
                 ,"skips 4" ~: [[True, False], [False]] ~=? skips [True, False]
                 ,"skips 5" ~: ([]::[[Int]]) ~=? skips []
                 ,"local_maxima 1" ~: ([9, 6]::[Int]) ~=? local_maxima [2, 9, 5, 6, 1]
                 ,"local_maxima 2" ~: ([4]::[Int])    ~=? local_maxima [2, 3, 4, 1, 5]
                 ,"local_maxima 3" ~: ([]::[Int])     ~=? local_maxima [1, 2, 3, 4, 5]
                 ,"local_maxima 4" ~: ([9]::[Int])    ~=? local_maxima [2, 9, 5]
                 ,"local_maxima 5" ~: ([]::[Int])     ~=? local_maxima [2, 9]
                 ,"local_maxima 6" ~: ([]::[Int])     ~=? local_maxima [9]
                 ,"local_maxima 7" ~: ([]::[Int])     ~=? local_maxima []
                 ,"hist_data 1" ~: [3, 0, 0, 0, 1, 0, 0, 0, 0, 0] ~=? hist_data [0, 0, 0, 4]
                 ,"hist_data 2" ~: [4, 1, 2, 1, 1, 1, 0, 0, 1, 0] ~=? hist_data [0, 4, 8, 1, 2, 0, 3, 2, 0, 5, 0]
                 ,"rotate" ~: ([[1,4,7],[2,5,8],[3,6,9]]::[[Int]]) ~=? rotate [[1,2,3], [4,5,6], [7,8,9]]
                 ,"histogram 1" ~: histogram_result1 ~=? histogram [0, 0, 0, 4]
                 ,"histogram 2" ~: histogram_result2 ~=? histogram [0, 4, 8, 1, 2, 0, 3, 2, 0, 5, 0]
                 ]


histogram_result1 :: String
histogram_result1 =
    "*         \n" ++
    "*         \n" ++
    "*   *     \n" ++
    "==========\n" ++
    "0123456789\n"


histogram_result2 :: String
histogram_result2 =
    "*         \n" ++
    "*         \n" ++
    "* *       \n" ++
    "******  * \n" ++
    "==========\n" ++
    "0123456789\n"
