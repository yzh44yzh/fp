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

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes codeSize = allCodes' codeSize [[]]

allCodes' :: Int -> [Code] -> [Code]
allCodes' 0 acc = acc
allCodes' codeSize acc =
    allCodes' (codeSize - 1) (acc >>= appendColors)
        where appendColors = \code -> map (\peg -> peg : code) colors



-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = solve' code (allCodes $ length code) []


solve' :: Code -> [Code] -> [Move] -> [Move]
solve' _ [] acc = acc
solve' code guesses acc =
    let guess : rest = guesses in
    let move = getMove code guess in
    let Move _ em _ = move in
    if em == (length code) then reverse $ move : acc
    else solve' code rest (move : acc)


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
                 ,"isConsistent 1" ~: True ~=? (isConsistent tMove tCode1)
                 ,"isConsistent 2" ~: False ~=? (isConsistent tMove tCode2)
                 ,"filterCodes 1" ~: [tCode1] ~=? filterCodes tMove [tCode1, tCode2]
                 ,"filterCodes 2" ~: [] ~=? filterCodes tMove [tCode2, tCode2]
                 ,"filterCodes 3" ~: [tCode1, tCode1] ~=? filterCodes tMove [tCode1, tCode1]
                 ,"allCodes 0" ~: [] ~=? allCodes 0
                 ,"allCodes 1" ~: [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]] ~=? allCodes 1
                 ,"allCodes 2" ~: [[Red, Red], [Green, Red], [Blue, Red], [Yellow, Red], [Orange, Red], [Purple, Red],
                                   [Red, Green], [Green, Green], [Blue, Green], [Yellow, Green], [Orange, Green], [Purple, Green],
                                   [Red, Blue], [Green, Blue], [Blue, Blue], [Yellow, Blue], [Orange, Blue], [Purple, Blue],
                                   [Red, Yellow], [Green, Yellow], [Blue, Yellow], [Yellow, Yellow], [Orange, Yellow], [Purple, Yellow],
                                   [Red, Orange], [Green, Orange], [Blue, Orange], [Yellow, Orange], [Orange, Orange], [Purple, Orange],
                                   [Red, Purple], [Green, Purple], [Blue, Purple], [Yellow, Purple], [Orange, Purple], [Purple, Purple]
                                  ] ~=? allCodes 2
                 ]


tMove :: Move
tMove = (Move [Red, Red, Blue, Green] 1 1)

tCode1 :: Code
tCode1 = [Red, Blue, Yellow, Purple]

tCode2 :: Code
tCode2 = [Red, Blue, Red, Purple]
