{-# OPTIONS_GHC -Wall #-}

validate :: Int -> Bool
validate cardNum =
    let code = (sumDigits . dobleOddPositions . num2digits) cardNum in
    code `rem` 10 == 0


num2digits :: Int -> [Int]
num2digits num =
    num2digits' num []


num2digits' :: Int -> [Int] -> [Int]
num2digits' num acc =
    if num < 10 then num : acc
    else num2digits' d (r : acc)
        where d = num `div` 10
              r = num `rem` 10


dobleOddPositions :: [Int] -> [Int]
dobleOddPositions =
    reverse . snd . foldl f a
        where a = (True, [])
              f = \acc val ->
                  case acc of
                    (True, lst) -> (False, (val * 2) : lst)
                    (False, lst) -> (True, val : lst)


sumDigits :: [Int] -> Int
sumDigits =
    foldl f 0
        where f = \acc val ->
                  if val > 10 then (sumDigits . num2digits) val + acc
                  else val + acc


main :: IO ()
main =
    let cardNum = 4012888888881881 in
    let res = validate cardNum in
    putStrLn $ (show cardNum) ++ " " ++ (show res)
