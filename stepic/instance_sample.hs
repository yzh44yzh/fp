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


test0 = lookup 42 (empty :: ListMap Int String)
test1 = lookup 42 $ ListMap [(1,"one"), (2, "two"), (42, "answer")]
test2 = insert 42 "answer" (empty :: ListMap Int String)
test3 = insert 42 "answer" $ ListMap [(1, "one")]
test4 = delete 42 $ ListMap [(1,"one"), (2, "two"), (42, "answer")]
