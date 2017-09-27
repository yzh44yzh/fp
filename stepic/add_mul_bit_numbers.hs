import Numeric
import Data.Char

data Bit = Zero | One deriving (Show, Eq)
data Sign = Minus | Plus deriving (Show, Eq)
data Z = Z Sign [Bit] deriving (Show, Eq)

-- add :: Z -> Z -> Z
-- add z1 z2 = integerToZ $ (zToInteger z1) + (zToInteger z2)


add :: Z -> Z -> Z
add z1 z2 =
    let (Z sign bits) = add' z1 z2 in
    Z sign $ skipZeros bits


add' :: Z -> Z -> Z
add' (Z Plus b1)  (Z Plus b2)  = Z Plus (uncurry addBits (align b1 b2) Zero)
add' (Z Minus b1) (Z Minus b2) = Z Minus (uncurry addBits (align b1 b2) Zero)
add' (Z Minus b1) (Z Plus b2)  = add' (Z Plus b2) (Z Minus b1)
add' (Z Plus b1)  (Z Minus b2) =
    let (b1', b2') = align b1 b2 in
    case compareBits (reverse b1') (reverse b2') of
      EQ -> Z Plus []
      GT -> Z Plus (subBits b1' b2' Zero)
      LT -> Z Minus (subBits b2' b1' Zero)


align :: [Bit] -> [Bit] -> ([Bit], [Bit])
align b1 b2 = (fill b1 (length b2), fill b2 (length b1))


fill :: [Bit] -> Int -> [Bit]
fill b size | diff <= 0 = b
            | otherwise = b ++ (take diff $ repeat Zero)
            where diff = size - length b


skipZeros :: [Bit] -> [Bit]
skipZeros = reverse . dropWhile (== Zero) . reverse


compareBits :: [Bit] -> [Bit] -> Ordering
compareBits [] [] = EQ
compareBits (One:_) (Zero:_) = GT
compareBits (Zero:_) (One:_) = LT
compareBits (_:t1) (_:t2) = compareBits t1 t2


addBits :: [Bit] -> [Bit] -> Bit -> [Bit]
addBits [] [] Zero = []
addBits [] [] One = [One]
addBits (h1:t1) (h2:t2) r =
    let (res, r2) = addBit h1 h2 r in
    res : addBits t1 t2 r2


addBit :: Bit -> Bit -> Bit -> (Bit, Bit)
addBit Zero Zero Zero = (Zero, Zero)
addBit Zero One  Zero = (One,  Zero)
addBit One  Zero Zero = (One,  Zero)
addBit One  One  Zero = (Zero, One)
addBit Zero Zero One  = (One,  Zero)
addBit Zero One  One  = (Zero, One)
addBit One  Zero One  = (Zero, One)
addBit One  One  One  = (One,  One)


subBits :: [Bit] -> [Bit] -> Bit -> [Bit]
subBits [] [] Zero = []
subBits [] [] One  = error "subBits"
subBits (h1:t1) (h2:t2) r =
    let (res, r2) = subBit h1 h2 r in
    res : subBits t1 t2 r2


subBit :: Bit -> Bit -> Bit -> (Bit, Bit)
subBit Zero Zero Zero = (Zero, Zero)
subBit Zero One  Zero = (One,  One)
subBit One  Zero Zero = (One,  Zero)
subBit One  One  Zero = (Zero, Zero)
subBit Zero Zero One  = (One,  One)
subBit Zero One  One  = (Zero, One)
subBit One  Zero One  = (Zero, Zero)
subBit One  One  One  = (One,  Zero)


mul :: Z -> Z -> Z
mul z1 z2 = integerToZ $ (zToInteger z1) * (zToInteger z2)


zToInteger :: Z -> Integer
zToInteger (Z _ []) = 0
zToInteger (Z sign bits) =
    case sign of
      Plus -> i
      Minus -> (-i)
    where
      i = foldr f 0 bits
      f :: Bit -> Integer -> Integer
      f One acc  = acc * 2 + 1
      f Zero acc = acc * 2


integerToZ :: Integer -> Z
integerToZ 0 = Z Plus []
integerToZ i | i > 0 = Z Plus b
             | i < 0 = Z Minus b
             where
               s = showIntAtBase 2 intToDigit (abs i) ""
               b = map f $ reverse s
               f '0' = Zero
               f '1' = One


test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]

testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058

test101 = (mul (Z Plus []) (Z Plus [])) == Z Plus []
test102 = (mul (Z Plus []) (Z Plus [One])) == Z Plus []
test103 = (mul (Z Plus []) (Z Minus [One])) == Z Plus []
test104 = (mul (Z Plus [One]) (Z Plus [])) == Z Plus []
test105 = (mul (Z Minus [One]) (Z Plus [])) == Z Plus []

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

testAll = testAdd && testMul
