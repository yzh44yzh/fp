http://www.seas.upenn.edu/~cis194/fall14/hw/04-poly.pdf

# Parametricity

Exercise 1 - 12 -- useless playing with types.


# Binary search trees

https://en.wikipedia.org/wiki/Binary_search_tree

A binary search tree is a recursive data stucture frequently used to
store a set — that is, a chunk of data that is easily (and
efficiently) searched and added to.

Here is the datatype definition for a binary search tree in Haskell

```
data BST a = Leaf | Node (BST a) a (BST a)
```

That is, a BST a is either a leaf (a placeholder that holds no data)
or an interior node, containing left and right sub-trees and a chunk
of data. The key property of a binary search tree is that the data in
a left sub-tree must all be less than or equal to the data in a node,
and that the data in a right sub-tree must all be greater than or
equal to the data in a node.

## Exercise 13

Write the insertion method for a binary search tree:

```
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
```

The first parameter to insertBST is a comparison function that takes
two a s and says what their relationship is. The next is the new
element to insert. Last we have the current tree.

It is interesting to note that, because of parametric polymorphism,
every call of insertBST must be accompanied by a comparison oper-
ation. Otherwise, there’s no way to know how to compare elements.
We’ll see a mechanism—called type classes — that will make this less
burdensome.


# Visiting the library

Use Data.List, Data.Maybe, and Data.Char to write succinct solutions.
Each of these functions has a simple, one-liner answer!


## Exercise 14

Check to see if a list of strings contains only capitalized words:

```
allCaps :: [String] -> Bool
```

Examples:

```
allCaps ["Hi","There"]   == True
allCaps []               == True
allCaps ["", "Blah"]     == False
allCaps ["Hi","there"]   == False
```


## Exercise 15

Drop the trailing whitespace from a string:

```
dropTrailingWhitespace :: String -> String
```

Examples:


```
dropTrailingWhitespace "foo"   == "foo"
dropTrailingWhitespace ""      == ""
dropTrailingWhitespace "bar  " == "bar"
```


## Exercise 16

Get the first letter (if it exists) of a list of strings:

```
firstLetters :: [String] -> [Char]
```

Examples:

```
firstLetters ["foo", "bar"] == [’f’,’b’]
firstLetters ["alpha",""]   == [’a’]
firstLetters []             == []
firstLetters ["",""]        == []
```


## Exercise 17

Render a proper bracketed list given a list of strings:

```
asList :: [String] -> String
```

Examples:

```
asList ["alpha","beta","gamma"] == "[alpha,beta,gamma]"
asList []                       == "[]"
asList ["lonely"]               == "[lonely]
```
