import Data.Char
import Data.List

data BST a = Leaf | Node (BST a) a (BST a) deriving (Show, Eq)


insertBST :: (Ord a) => a -> BST a -> BST a
insertBST val Leaf = Node Leaf val Leaf
insertBST val tree@(Node left curr right) =
    case compare val curr of
      EQ -> tree
      LT -> Node (insertBST val left) curr right
      GT -> Node left curr (insertBST val right)


allCaps :: [String] -> Bool
allCaps lst = all isCaps lst

isCaps :: String -> Bool
isCaps [] = False
isCaps (c : _) = isUpper c


dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . (dropWhile isSpace) . reverse


firstLetters :: [String] -> [Char]
firstLetters = foldl f []
    where f :: [Char] -> String -> String
          f acc [] = acc
          f acc (c : _) = c : acc


asList :: [String] -> String
asList lst = "[" ++ (concat $ intersperse "," lst) ++ "]"
