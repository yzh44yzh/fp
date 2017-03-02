# Scrabble

It’s time to have some fun!

You will be writing functions to help in a computer player for a
Scrabble game. Though familiarity with the rules is not assumed for
this assignment, you may wish to read them at
http://www.hasbro.com/scrabble/en_US/discover/rules.cfm

We will be using some type definitions to make this work nicely.
Please download the HW02.hs file (linked from the “Lectures” page) and
edit that file. You will also need the file Words.hs, which defines a
list of all possible Scrabble words. Make sure these files are in the
same directory, so that the files can see each other.

Many of the functions return lists of words. The order of the words
within the list does not matter.


## Exercise 1

The first function you will write tests whether a certain word is
formable from the tiles in a Scrabble hand. That is, given a
**String** and a list of **Char**s, can the **String** be formed from
the **Char**s, taking any duplicates into account?

As described in HW02.hs, we use a type synonym
```
type Hand = [Char]
```
to talk about Scrabble hands, to make it very clear where order
matters (in words) and where it does not (in hands).
```
formableBy :: String -> Hand -> Bool
```
Example: formableBy "fun" [’x’,’n’,’i’,’f’,’u’,’e’,’l’] == True
Example: formableBy "haskell" [’k’,’l’,’e’,’h’,’a’,’l’,’s’] == True
Example: formableBy "haskell" [’k’,’l’,’e’,’h’,’a’,’y’,’s’] == False

Hint: Start by thinking what this should do if the string to be
matched is empty. Then, what should it do if the string is non-empty?
The **elem** and **delete** functions from **Data.List** may be
helpful here.


## Exercise 2

Now, using **formableBy**, write a function **wordsFrom** that gives
a list of all valid Scrabble words formable from a certain hand. The
**Words** module (imported by the **HW02.hs** you downloaded) allows
you to use
```
allWords :: [String]
```
which contains all valid Scrabble words.
```
wordsFrom :: Hand -> [String]
```

Example: wordsFrom [’a’,’b’,’c’,’d’] == ["ab","ad","ba","bad","cab","cad","dab"]
Example: wordsFrom [’h’,’e’,’l’,’l’,’o’] ==
    [ "eh","el","ell","he","hell","hello","helo"
    , "ho","hoe","hole","lo","oe","oh","ole" ]

This function will likely require a helper function in order to
process all of the elements in allWords.


## Exercise 3

Most plays in Scrabble do not build completely fresh words from the
tiles in one’s hand. All Scrabble plays (except the first) have to
build on existing tiles. Often, there is a place that a player wants
to make a word, but that player must figure out if a word can
fit. Your next functions will help to solve this part of the problem.

First, we must have the idea of a **template**. A template is a string
containing some letters and some question marks. The question marks
represent open spaces on the board that will get filled in by the
letter in a player’s hand. The letters in the template represent
letters that already appear on the board. They must appear in exactly
the same positions in the final words produced. So, the template
**??r?** represents a board position where the third letter of a
four-letter word must be **r**. If you have the letters available, you
could play **care** or **burp** there (among many other words).

Write a function **wordFitsTemplate** that checks to see if a given
word matches a template, given a set of tiles available:
```
wordFitsTemplate :: Template -> Hand -> String -> Bool
```

Example: wordFitsTemplate "??r?" [’c’,’x’,’e’,’a’,’b’,’c’,’l’] "care" == True
Example: wordFitsTemplate "??r?" [’c’,’x’,’e’,’w’,’b’,’c’,’l’] "care" == False
Example: wordFitsTemplate "??r?" [’c’,’x’,’e’,’a’,’b’,’c’,’l’] "car" == False
Example: wordFitsTemplate "let" [’x’,’x’] "let" == True


## Exercise 4

Now, using that function, write another one that produces all valid
Scrabble words that match a given template using a hand of available
tiles. This will be similar to **wordsFrom**.
```
wordsFittingTemplate :: Template -> Hand -> [String]
```

Example : wordsFittingTemplate "??r?" [’c’,’x’,’e’,’a’,’b’,’c’,’l’] ==
    ["acre","bare","carb","care","carl","earl"]


## Exercise 5

Now we must think about scoring, as not all words in Scrabble are
created equal! The **Words** module, along with providing
**allWords**, provides
```
scrabbleValue :: Char -> Int
```
that gives the point value of any letter. Use that function to write a
new function that gives the point value of any word.
```
scrabbleValueWord :: String -> Int
```

Example: scrabbleValueWord "care" == 6
Example: scrabbleValueWord "quiz" == 22


## Exercise 6

You will use the **scrabbleValueWord** function to write a filtering
function that takes a list of words and selects out only those that
have the maximum point value. Note that there may be many words tied
for the most points; your function must return all of them.
```
bestWords :: [String] -> [String]
```

Example: bestWords (wordsFittingTemplate "??r?" [’c’,’x’,’e’,’a’,’b’,’c’,’l’]) == ["carb"]
Example: bestWords ["cat", "rat", "bat"] == ["bat","cat"]
Example: bestWords [] == []

A helper function with an accumulator may come in handy here, but
there are other possible solutions.


## Exercise 7

A Scrabble board is not a completely blank canvas. There are four
kinds of special squares: double-letter, triple-letter, double-word,
and triple-word. A letter played on a double- or triple-letter square
gets its point value multiplied, and if any letter is played on a
double- or triple-word square, the whole word’s value gets
multiplied. The effects multiply with each other, as appropriate. So,
a play on both a double-word and a triple-word gets multiplied by 6.
If one tile is on a double-letter and another is on a double-word,
then that letter’s value is multiplied by 4.

To represent these special squares, we use a new type **STemplate**
(the S is for "square"). A stemplate is like a template, but it uses
’D’ and ’T’ to mark double- and triple-letter squares, respectively,
and it uses ’2’ and ’3’ to mark double- and triple-word squares,
respectively. Thus, the stemplate **?e??3** represents a place on the
board where there is room for a 5-letter word, the second letter of
which is an **e**, and the last letter of which falls on a triple-word
square.

Write a function **scrabbleValueTemplate** that computes the value of
playing a given word on a given template. In this function, you may
assume that the word actually matches the template.
```
scrabbleValueTemplate :: STemplate -> String -> Int
```

Example: scrabbleValueTemplate "?e??3" "peace" == 27
Example: scrabbleValueTemplate "De?2?" "peace" == 24
Example: scrabbleValueTemplate "??Tce" "peace" == 1
