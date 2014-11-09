-- {-# OPTIONS_GHC -Wall #-}

-- Exercises from
-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
-- Repeating the questions part in this file, for easy read.

import Data.List

-- Write a function that computes the number of elements in a list. 
-- To test it, ensure that it gives the same answers as the standard 
-- length function. Add a type signature for your function to your source file. 
-- To test it, load the source file into ghci again.
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

testListLength :: [a] -> Bool
testListLength xs
    | length xs == listLength xs    = True
    | otherwise                     = False
    
-- Write a function that computes the mean of a list, i.e. the sum of all 
-- elements in the list divided by its length. (You may need to use the 
-- fromIntegral function to convert the length of the list from an integer into
-- a floating point number.)    
mean :: (Fractional a) => [a] ->  a
mean xs = sum xs / fromIntegral (length xs)

-- Turn a list into a palindrome, i.e. it should read the same both backwards 
-- and forwards. For example, given the list [1,2,3], your function should 
-- return [1,2,3,3,2,1].
makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome xs = xs ++ reverse xs

-- Write a function that determines whether its input list is a palindrome.
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome xs = xs == (reverse xs)

-- Create a function that sorts a list of lists based on the length of each 
-- sublist. (You may want to look at the sortBy function from the Data.List 
-- module.)
sortLen xs ys = compare (length xs) (length ys)
sortListLen = sortBy sortLen

-- Define a function that joins a list of lists together using a separator value
-- intersperse' ',' ["foo","bar","baz","quux"] == "foo,bar,baz,quux"
intersperse' :: a -> [[a]] -> [a]
intersperse' x ys = intercalate [x] ys


-- Using the binary tree type that we defined earlier in this chapter, write a 
-- function that will determine the height of the tree. The height is the 
-- largest number of hops from the root to an Empty. For example, the tree 
-- Empty has height zero; Node "x" Empty Empty has height one; 
-- Node "x" Empty (Node "y" Empty Empty) has height two; and so on
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- treeHeight :: Tree -> Int              


-- Consider three two-dimensional points a, b, and c. If we look at the angle 
-- formed by the line segment from a to b and the line segment from b to c, it 
-- either turns left, turns right, or forms a straight line. Define a Direction 
-- data type that lets you represent these possibilities.
data Direction = Lt | St | Rt deriving (Show, Eq, Ord)

-- Write a function that calculates the turn made by three 2D points and 
-- returns a Direction.
data Cartesian2D = Cartesian2D {x :: Double, y :: Double}
                   deriving (Eq, Show)

-- Look into http://en.wikipedia.org/wiki/Graham_scan for more details                   
getDirection :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
getDirection a b c 
   | crsprd > 0  = Lt
   | crsprd == 0 = St
   | crsprd < 0  = Rt
   where crsprd =  (x b - x a) * (y c - y a) - (y b - y a) * (x c - x a)
   
-- Define a function that takes a list of 2D points and computes the direction 
-- of each successive triple. Given a list of points [a,b,c,d,e], it should 
-- begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], 
-- then [c,d,e]. Your function should return a list of Direction.   
getDirList :: [Cartesian2D] -> [Direction]
getDirList (a:b:c:xs) = (getDirection a b c) : getDirList (b:c:xs)
getDirList _          = []

-- Using the code from the preceding three exercises, implement Graham's scan 
-- algorithm for the convex hull of a set of 2D points. You can find good 
-- description of what a convex hull. is, and how the Graham scan algorithm 
-- should work, on Wikipedia.
