{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (lookup)
import qualified Data.List as L


class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)


newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)


instance MapLike ListMap where

    empty :: ListMap k v
    empty = ListMap []

    lookup :: Ord k => k -> ListMap k v -> Maybe v
    lookup key (ListMap list) = foldr f Nothing list
        where f _ (Just res) = Just res
              f (k,v) Nothing = if k == key then Just v else Nothing

    insert :: Ord k => k -> v -> ListMap k v -> ListMap k v
    insert key value map =
        let ListMap list = delete key map in
        ListMap $ (key, value) : list

    delete :: Ord k => k -> ListMap k v -> ListMap k v
    delete key (ListMap list) = ListMap $ foldr f [] list
        where f (k,v) acc = if k == key then acc else (k,v) : acc

    fromList :: Ord k => [(k,v)] -> ListMap k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)


newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where

    empty :: ArrowMap k v
    empty = ArrowMap (\_ -> Nothing)

    lookup :: Ord k => k -> ArrowMap k v -> Maybe v
    lookup key map = getArrowMap map $ key

    insert :: Ord k => k -> v -> ArrowMap k v -> ArrowMap k v
    insert key value map =
        let f = getArrowMap map in
        -- case f key of
          -- Just v | v == value -> map -- need (Eq v) here
          -- _ -> ArrowMap (\k -> if k == key then Just value else f k)
        ArrowMap (\k -> if k == key then Just value else f k)

    delete :: Ord k => k -> ArrowMap k v -> ArrowMap k v
    delete key map =
        let f = getArrowMap map in
        case f key of
          Nothing -> map
          Just _ -> ArrowMap (\k -> if k == key then Nothing else f k)

    fromList :: Ord k => [(k,v)] -> ArrowMap k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)


sample :: ArrowMap Int String
sample = fromList [(1,"one"), (2, "two"), (42, "answer")] -- :: ArrowMap Int String


test0 = Nothing == lookup 42 (empty :: ArrowMap Int String)
test1 = Just "answer" == (lookup 42 sample)
test2 = Nothing == (lookup 42 $ delete 42 sample)
test3 = Just "answer" == (lookup 42 $ insert 42 "answer" (empty :: ArrowMap Int String))
test4 = Just "one" == (lookup 1 $ insert 42 "answer" $ (fromList [(1, "one")] :: ArrowMap Int String))
test5 = Just "three" == (lookup 3 $
                                insert 4 "four" $
                                       insert 3 "three" $
                                              insert 2 "two" $
                                                         (empty :: ArrowMap Int String))
test6 = Nothing == (lookup 1 $ delete 1 $ sample)
test7 = Just "one" == (lookup 1 $ insert 1 "one" $ delete 1 $ sample)
test8 = Just "question" == (lookup 42 $ insert 42 "question" $ sample)

test = test0 && test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8
