import qualified Data.List as L
import Test.HUnit


data Poly a = Poly [a] deriving Eq


(|>) :: a -> (a -> b) -> b
(|>) a f = f a


-- Exercise 1 -- don't understand
-- Exercise 2 -- don't understand


-- instance Show Poly where
--     show (Poly lst) = reverse $ zip lst [1..]


showPoly :: (Eq a, Show a, Num a) => Poly a -> String
showPoly (Poly [0]) = "0"
showPoly (Poly lst) =
    zip lst [0..] |> reverse |> filter nonZeroCoef |> map showItem |> L.intersperse " + " |> L.concat
        where nonZeroCoef :: (Eq a, Num a) => (a, Integer) -> Bool
              nonZeroCoef (coef, _) = coef /= 0
              showItem :: (Eq a, Show a, Num a) => (a, Integer) -> String
              showItem (coef, 0) = show coef
              showItem (1, 1) = "x"
              showItem (coef, 1) = (show coef) ++ "x"
              showItem (1, degree) = "x^" ++ (show degree)
              showItem (coef, degree) = (show coef) ++ "x^" ++ (show degree)



-- runTestTT tests
-- https://github.com/hspec/HUnit
tests :: Test
tests = TestList ["showPoly" ~: "6x^2 + 4x + 2"  ~=? (showPoly (Poly [2, 4, 6]))
                 ,"showPoly" ~: "10x^3 + 9x^2 + 8x + 7"  ~=? (showPoly (Poly [7, 8, 9, 10]))
                 ,"showPoly" ~: "3x^2 + 2x + 1" ~=? (showPoly (Poly [1, 2, 3]))
                 ,"showPoly" ~: "3x^2 + 1" ~=? (showPoly (Poly [1, 0, 3]))
                 ,"showPoly" ~: "3x^2 + x + 1" ~=? (showPoly (Poly [1, 1, 3]))
                 ,"showPoly" ~: "2x + 1" ~=? (showPoly (Poly [1, 2, 0]))
                 ,"showPoly" ~: "x^2 + 2x + 1" ~=? (showPoly (Poly [1, 2, 1]))
                 ,"showPoly" ~: "" ~=? (showPoly (Poly []))
                 ,"showPoly" ~: "0" ~=? (showPoly (Poly [0]))
                 ,"showPoly" ~: "1" ~=? (showPoly (Poly [1]))
                 ,"showPoly" ~: "2x + 1" ~=? (showPoly (Poly [1, 2]))
                 ,"showPoly" ~: "6x^5 + 5x^4 + 4x^3 + 3x^2 + 2x + 1" ~=? (showPoly (Poly [1, 2, 3, 4, 5, 6]))
                 ,"showPoly" ~: "3x^2 + 2x + -1" ~=? (showPoly (Poly [-1, 2, 3]))
                 ,"showPoly" ~: "3x^2 + -2x + 1" ~=? (showPoly (Poly [1, -2, 3]))
                 ,"showPoly" ~: "-3x^2 + 2x + 1" ~=? (showPoly (Poly [1, 2, -3]))
                 ,"showPoly" ~: "-3x^2 + -2x + -1" ~=? (showPoly (Poly [-1, -2, -3]))
                  ]

-- TODO:
-- negative coefficients. For example, 2x^2 + -3 , is the correct representation of 2x^2 − 3.
