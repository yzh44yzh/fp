module Scrabble where

import Words as W
import Data.List as L
import Test.HUnit

type Hand = [Char]
type Word = String
type Template = String
type STemplate = String

formableBy :: Word -> Hand -> Bool
formableBy [] hand = True
formableBy (ch : rest) hand =
    if L.elem ch hand
    then formableBy rest (L.delete ch hand)
    else False


wordsFrom :: Hand -> [Word]
wordsFrom hand = filter (formableBy hand) W.allWords


wordFitsTemplate :: Word -> Template -> Bool
wordFitsTemplate [] [] = True
wordFitsTemplate [] _ = False
wordFitsTemplate _ [] = False
wordFitsTemplate (w : word) (t : tpl)
    | t == '?' = wordFitsTemplate word tpl
    | w == t = wordFitsTemplate word tpl
    | otherwise = False


-- runTestTT tests
-- https://github.com/hspec/HUnit
tests =
    TestList
        ["formableBy" ~: TestList
            ["formableBy 1" ~: True  ~=? (formableBy "fun" ['x','n','i','f','u','e','l'])
            ,"formableBy 2" ~: True  ~=? (formableBy "haskell" ['k','l','e','h','a','l','s'])
            ,"formableBy 3" ~: False ~=? (formableBy "haskell" ['k','l','e','h','a','y','s'])
            ]
        ,"wordFitsTemplate" ~: TestList
            ["wordFitsTemplate 1" ~: True  ~=? (wordFitsTemplate "hello" "he???")
            ,"wordFitsTemplate 2" ~: False ~=? (wordFitsTemplate "hello" "he????")
            ,"wordFitsTemplate 2" ~: True  ~=? (wordFitsTemplate "he" "he")
            ,"wordFitsTemplate 2" ~: True  ~=? (wordFitsTemplate "hel" "???")
            ,"wordFitsTemplate 2" ~: False ~=? (wordFitsTemplate "allo" "a??a")
            ]
        ]
