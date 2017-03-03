module Scrabble where

import Words as W
import Data.List
import Test.HUnit

type Hand = [Char]
type Word = String
type Template = String
type STemplate = String

formableBy :: Word -> Hand -> Bool
formableBy [] hand = True
formableBy (ch : rest) hand =
    if ch `elem` hand
    then formableBy rest (delete ch hand)
    else False


wordsFrom :: Hand -> [Word]
wordsFrom hand =
    filter (formableBy hand) W.allWords


wordFitsTemplate :: Word -> Template -> Bool
wordFitsTemplate [] [] = True
wordFitsTemplate [] _ = False
wordFitsTemplate _ [] = False
wordFitsTemplate (w : word) (t : tpl)
    | t == '?' = wordFitsTemplate word tpl
    | w == t = wordFitsTemplate word tpl
    | otherwise = False


handAndWordFitsTemplate :: Hand -> Template -> Word -> Bool
handAndWordFitsTemplate hand tpl word =
    wordFitsTemplate word tpl && formableBy word fullHand
        where fullHand = hand ++ filter (/= '?') tpl


wordsFittingTemplate :: Hand -> Template -> [Word]
wordsFittingTemplate hand tpl =
    filter (handAndWordFitsTemplate hand tpl) W.allWords


scrabbleValueWord :: Word -> Int
scrabbleValueWord word =
    foldl (\acc ch -> acc + W.scrabbleValue ch) 0 word


bestWords :: [Word] -> [Word]
bestWords [] = []
bestWords words =
    selectBest candidats bestValue []
    where fm = \w -> (w, scrabbleValueWord w)
          fs = \(_,v1) (_,v2) -> compare v2 v1
          candidats = sortBy fs $ map fm words
          bestValue = snd $ head candidats
          selectBest = \words value acc ->
                       case words of
                         (w,v) : rest | v == value -> selectBest rest value (w : acc)
                         _ -> acc


scrabbleValueTemplate :: STemplate -> Word -> Int
scrabbleValueTemplate tpl word =
    trippleWord . doubleWord . (foldl calcChars 0) $ zip tpl word
    where calcChars = \acc (tc, wc) ->
                      case tc of
                        '2' -> acc + W.scrabbleValue wc * 2
                        '3' -> acc + W.scrabbleValue wc * 3
                        _ -> acc + W.scrabbleValue wc
          doubleWord = \value -> if 'D' `elem` tpl then 2 * value else value
          trippleWord = \value -> if 'T' `elem` tpl then 3 * value else value


-- runTestTT tests
-- https://github.com/hspec/HUnit
tests = TestList ["formableBy 1" ~: True  ~=? (formableBy "fun" ['x','n','i','f','u','e','l'])
                 ,"formableBy 2" ~: True  ~=? (formableBy "haskell" ['k','l','e','h','a','l','s'])
                 ,"formableBy 3" ~: False ~=? (formableBy "haskell" ['k','l','e','h','a','y','s'])
                 ,"wordFitsTemplate 1" ~: True  ~=? (wordFitsTemplate "hello" "he???")
                 ,"wordFitsTemplate 2" ~: False ~=? (wordFitsTemplate "hello" "he????")
                 ,"wordFitsTemplate 3" ~: True  ~=? (wordFitsTemplate "he" "he")
                 ,"wordFitsTemplate 4" ~: True  ~=? (wordFitsTemplate "hel" "???")
                 ,"wordFitsTemplate 5" ~: False ~=? (wordFitsTemplate "allo" "a??a")
                 ,"handAndWordFitsTemplate 1" ~: True  ~=? handAndWordFitsTemplate ['c','x','e','a','b','c','l'] "??r?" "care"
                 ,"handAndWordFitsTemplate 2" ~: False ~=? handAndWordFitsTemplate ['c','x','e','w','b','c','l']"??r?" "care"
                 ,"handAndWordFitsTemplate 3" ~: False ~=? handAndWordFitsTemplate ['c','x','e','a','b','c','l']"??r?" "car"
                 ,"handAndWordFitsTemplate 4" ~: True  ~=? handAndWordFitsTemplate  ['x','x'] "let" "let"
                 ,"wordsFittingTemplate"
                      ~: ["acre","bare","carb","care","carl","earl"]
                      ~=? wordsFittingTemplate ['c', 'x', 'e', 'a', 'b', 'c', 'l'] "??r?"
                 ,"scrabbleValueWord 1" ~: 6  ~=? scrabbleValueWord "care"
                 ,"scrabbleValueWord 2" ~: 22 ~=? scrabbleValueWord "quiz"
                 ,"bestWords 1" ~: [] ~=? bestWords []
                 ,"bestWords 2" ~: ["carb"] ~=? (bestWords $ wordsFittingTemplate ['c', 'x', 'e', 'a', 'b', 'c', 'l'] "??r?")
                 ,"bestWords 3" ~: ["bat", "cat"] ~=? bestWords ["cat", "rat", "bat"]
                 ,"scrabbleValueTemplate 1"  ~: 11 ~=? scrabbleValueTemplate "?e??3" "peace"
                 ,"scrabbleValueTemplate 2"  ~: 24 ~=? scrabbleValueTemplate "De?2?" "peace"
                 ,"scrabbleValueTemplate 3"  ~: 27  ~=? scrabbleValueTemplate "??Tce" "peace"
                 ]
