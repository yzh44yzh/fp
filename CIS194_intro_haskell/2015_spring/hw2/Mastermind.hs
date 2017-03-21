{-# OPTIONS_GHC -Wall #-}
module Mastermind where

import Test.HUnit

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches code guess =
    foldl f 0 $ zip code guess
        where f = \acc (c, v) ->
                  if c == v then acc + 1
                  else acc



-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code =
    map (\color -> countColor color code) colors

countColor :: Peg -> Code -> Int
countColor color code =
    foldl (\acc peg -> if peg == color then acc + 1 else acc) 0 code


-- Count number of matches between the actual code and the guess
totalMatches :: Code -> Code -> Int
totalMatches code guess =
    foldl f 0 $ zip codeColors guessColors
        where codeColors = countColors code
              guessColors = countColors guess
              f = \acc (cc, gc) -> acc + min cc gc

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove code guess =
    Move guess em (tm - em)
        where em = exactMatches code guess
              tm = totalMatches code guess


-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move code =
    let Move guess em nem = move in
    let em' = exactMatches code guess in
    let tm' = totalMatches code guess in
    em == em' && nem == (tm' - em')


-- Exercise 5 -----------------------------------------

-- filterCodes :: Move -> [Code] -> [Code]
-- filterCodes = undefined

-- Exercise 6 -----------------------------------------

-- allCodes :: Int -> [Code]p
-- allCodes = undefined

-- Exercise 7 -----------------------------------------

-- solve :: Code -> [Move]
-- solve = undefined

-- Bonus ----------------------------------------------

-- fiveGuess :: Code -> [Move]
-- fiveGuess = undefined


-- runTestTT tests
-- https://github.com/hspec/HUnit
tests :: Test
tests = TestList ["exactMatches 0" ~: 0  ~=? (exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red])
                 ,"exactMatches 1" ~: 1  ~=? (exactMatches [Red, Blue, Green, Yellow] [Green, Purple, Green, Green])
                 ,"exactMatches 2" ~: 2  ~=? (exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange])
                 ,"exactMatches 3" ~: 3  ~=? (exactMatches [Red, Blue, Green, Yellow] [Red, Blue, Red, Yellow])
                 ,"exactMatches 4" ~: 4  ~=? (exactMatches [Red, Blue, Green, Yellow] [Red, Blue, Green, Yellow])
                 ,"countColors 1" ~: [1, 0, 1, 1, 0, 1] ~=? (countColors [Red, Blue, Yellow, Purple])
                 ,"countColors 2" ~: [0, 2, 1, 0, 1, 0] ~=? (countColors [Green, Blue, Green, Orange])
                 ,"totalMatches" ~: 3 ~=? (totalMatches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue])
                 ,"getMove" ~: Move [Red, Orange, Orange, Blue] 1 2 ~=?
                      (getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue])
                 ,"isConsistent 1" ~: True ~=?
                      (isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple])
                 ,"isConsistent 2" ~: False ~=?
                      (isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple])
                 ]
