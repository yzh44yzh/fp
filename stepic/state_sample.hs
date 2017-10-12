import Control.Applicative
import Control.Monad (liftM, ap)


newtype State s a = State {runState :: s -> (a,s) }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
    return a = State $ \st -> (a,st)
    m >>= k = State $ \st ->
              let (a, st') = runState m st
                  m' = k a
              in runState m' st'


execState :: State s a -> s -> s
execState m s = snd (runState m s)


evalState :: State s a -> s -> a
evalState m s = fst (runState m s)


get :: State s s
get = State $ \st -> (st, st)


put :: s -> State s ()
put st = State $ \_ -> ((), st)


data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show


numberTree :: Tree () -> Tree Integer
numberTree t = evalState (numberTree' t) 1


numberTree' :: Tree () -> State Integer (Tree Integer)
numberTree' (Leaf ())= do
    i <- get
    put (i + 1)
    return $ Leaf i
numberTree' (Fork l () r) = do
    l' <- numberTree' l
    i <- get
    put (i + 1)
    r' <- numberTree' r
    return $ Fork l' i r'


test1 = numberTree (Leaf ())
-- Leaf 1
test2 = numberTree (Fork (Leaf ()) () (Leaf ()))
-- Fork (Leaf 1) 2 (Leaf 3)
