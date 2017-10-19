-- Вспомним, что такое функтор для функции

-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))
--     или
--     fmap = (.)

test0 = fmap (*3) (+100) 1
-- 303


-- Прежде, чем делать Arr2 и Arr3, сделаем вещь попроще -- Arr1

newtype Arrow a b = Arrow { getArrow :: a -> b }

a1 :: Arrow Int Int
a1 = Arrow (*3)

a2 :: Arrow Int Int
a2 = Arrow (+100)

instance Functor (Arrow b) where
    fmap f arr = Arrow $ f . (getArrow arr)

test1 = (getArrow $ fmap (+1) a1) 10
-- 31

test2 = (getArrow $ fmap (*10) a2) 10
-- 1100


-- Теперь можно взяться за Arr2

newtype Arr2 arg1_type arg2_type reply_type = Arr2 { getArr2 :: arg1_type -> arg2_type -> reply_type }

aa :: Arr2 Int String String
aa = Arr2 take

aa_test = (getArr2 aa) 5 "abcdef"
-- "abcde"


instance Functor (Arr2 arg2_type reply_type) where
    fmap f arr = let f2 = getArr2 arr in
                 Arr2 $ (\x -> f . (f2 x))


aa_test2 = getArr2 (fmap length aa) 10 "abc"
-- 3



-- А теперь можно взяться за Arr3

newtype Arr3 a1t a2t a3t rt = Arr3 { getArr3 :: a1t -> a2t -> a3t -> rt }

aaa :: Arr3 (Int -> Int -> Int) [Int] [Int] [Int]
aaa = Arr3 zipWith

aaa_test = getArr3 aaa (+) [1,2,3,4] [10,20,30,40,50]
-- [11, 22, 33, 44]


instance Functor (Arr3 a2t a3t rt) where
    fmap f arr = let f3 = getArr3 arr in
                 Arr3 $ (\x y -> f . (f3 x y))


aaa_test2 =  getArr3 (tail <$> tail <$> aaa) (+) [1,2,3,4] [10,20,30,40,50]
-- -- [33,44]


-- Аппликативный функтор

-- instance Applicative ((->) a) where
--     pure x = \e -> x
--     (<*>) :: (e -> (a -> b)) -> (e -> a) -> e -> b
--     (<*>) g h = \e -> g e (h e)


af_test0 = (+) <*> (*3) $ 2
-- 8

-- g = (+) :: e -> a -> b
-- h == (*3) :: e -> a
-- g <*> h = \e -> (+) e ((*3) e) = (+e) (e*3) = e * 3 + e


instance Applicative (Arrow e) where
   pure x = Arrow $ \e -> x
   -- (<*>) :: Arr e (a -> b) -> Arr e a -> Arr e b
   (<*>) a1 a2 = Arrow $ \e -> f1 e (f2 e)
       where f1 = getArrow a1
             f2 = getArrow a2


toString :: Int -> String
toString x = show x


af_test1 = getArrow (Arrow (+) <*> Arrow (*3)) 5
-- 20

af_test2 = getArrow (Arrow (\x y -> (x,y)) <*> Arrow (toString)) 4
-- (4,"4")


instance Applicative (Arr2 e2 a) where
   pure x = Arr2 $ \e1 e2 -> x
   (<*>) a1 a2 = Arr2 $ \e1 e2 -> f1 e1 e2 (f2 e1 e2)
       where f1 = getArr2 a1
             f2 = getArr2 a2


af_test3 = getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3
-- -1


instance Applicative (Arr3 e2 e3 a) where
   pure x = Arr3 $ \e1 e2 e3 -> x
   (<*>) a1 a2 = Arr3 $ \e1 e2 e3 -> f1 e1 e2 e3 (f2 e1 e2 e3)
       where f1 = getArr3 a1
             f2 = getArr3 a2


af_test4 = getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4
-- -15
