-- {-# OPTIONS_GHC -Wall #-}

-- Chapter 3. Defining Types, Streamlining Functions    
-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
import Data.List

-- BookInfo datatype is composed of an Int, a String and a list of strings
-- possibly, we may use that for S.No, Book Name and a list of authors of book
-- each of these(Int, String, [String]) can be called a field
data BookInfo = Book Int String [String] deriving Show
-- "BookInfo" is the data type, defined using "data" keyword
-- "BookInfo" is called the type constructor
-- The variables of type "BookInfo" are referred type constructor "BookInfo"
-- "Book" is the value constructor (or data constructor)
-- "Book" is used to create a new value of type "BookInfo"

data MagazineInfo = Magazine Int String [String] deriving Show
-- Though BookInfo and MagazineInfo look very similar, contain identical fields,
-- Haskell's type system ensures that these two CANNOT be used interchangeably

-- The value constructors "Book" and "Magazine" are functions themselves
-- Of the following types
-- Book :: Int -> String -> [String] -> BookInfo
-- Magazine :: Int -> String -> [String] -> MagazineInfo
-- Book takes 3 arguments and return an object of type "BookInfo"
-- Magazine takes 3 arguments and return an object of type "MagazineInfo"

-- In GHCI:
-- ":info <datatype>" command gives detailed information about the datatype
-- ":type <fn or object>" command gives the type of the expression / function

-- The type constructor and value constructor can be of the same name
type CustomerID = Int               -- creating a type synonymn
-- Type Synonym is just a new name for an existing (also user created) data type
-- Int and CustomerID can be used interchangeably
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody
-- Because they are used in a different context, no ambiguity
-- Try ":t BookReview" in ghci
-- BookReview :: BookInfo -> CustomerID -> String -> BookReview
-- In the above type signature, the first BookReview refers to the value
-- constructor (the function that is used to create a new value), and the final
-- BookReview refers to the type of the value that just got created

-- Algebraic Data Types
-- A data type with more than one value constructor
-- data Bool = False | True
-- Both value constructors take zero arguments in the Bool example

-- Each of the value constructor in an algebraic data type can take different
-- arguments (zero or more)
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                    | CashOnDelivery
                    | Invoice CustomerID
                    deriving (Show)
                    
-- CreditCard, CashOnDelivery and Invoice - are all value constructors that
-- create a value of type "BillingInfo"; Each takes different number of args


-- Algebraic data type helps in differentiating otherwise identical info
x = ("WACA", "Gabba")                   -- cricket grounds
y = ("Chandrayaan", "Mangalyaan")       -- space probes
-- both x and y have same types here, which can be disitinguished for 
-- readability and for avoid mistreating one to be another

data CricketGround = CricketGround String String
data SpaceProbe = SpaceProbe String String

-- Finer Example, here
-- data types can be thought of as similar to Structure in C language

-- x and y lengths
data Cartesian2D = Cartesian2D Double Double deriving (Eq, Show)
-- angle and distance from origin
data Polar2D = Polar2D Double Double deriving (Eq, Show)

-- The following expression throws up a type error
-- Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2

-- An alternate set of data types
data Cartesian2D' = Cartesian Double Double 
                    | PolarToCartesian Polar2D'
                    deriving (Eq, Show)

data Polar2D' = Polar Double Double
                | CartesianToPolar Cartesian2D'
                deriving (Eq, Show)

-- Comparing the Haskell data type with C "struct"

-- "C" Language struct definition for book_info
-- struct book_info {
--    int id;
--    char *name;
--    char **authors;
-- };

-- Haskell definition
-- data BookInfo = Book Int String [String]
--                 deriving (Show)
                
-- In Haskell, the fields are anonymous and positional
-- anonymous, because the fields are not identified by separate names
-- (though, there is record syntax, which helps in naming)
-- positional, because thef ields are identified by a fixed position

-- Algebraic data type can also be thought of as enumeration eqv. in C
{- 
    "C" code starts
    enum roygbiv {
        red,
        orange,
        yellow,
        green,
        blue,
        indigo,
        violet,
    };
    "C" code ends
-}

data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)

-- enum can be used in C, in place of int
-- Haskell, being strongly typed, prevents such use, which can be potential 
-- cause of bugs

-- Another "C" equivalent code
{-
    enum shape_type {
        shape_circle,
        shape_poly,
    };

    struct vector {
        float x;
        float y;
    }

    struct circle {
        struct vector centre;
        float radius;
    };

    struct poly {
        size_t num_vertices;
        struct vector *vertices;
    };

    struct shape 
    {
        enum shape_type type;
        union {
            struct circle circle;
            struct poly poly;
        } shape;
    };
-}


-- "shape" is a C union which can contain struct circle or struct poly
-- We need to maintain a separate identifier "shape_type" to identify the same!
-- This could be potential source of bugs, if not updated properly

-- Haskell equivalent:
-- Much shorter, and beautifully elegant!
type Vector = (Double, Double)
data Shape = Circle Vector Double | Poly [Vector]
-- Shape is the data type (type constructor)
-- Circle and Poly are value constructors which create a value of type "Shape"

λ = 5
அருண் = 10
               
-- Exercises from
-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
-- Repeating the questions part in this file, for easy read.


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

-- Look into http://en.wikipedia.org/wiki/Graham_scan for more details                   
--getDirection :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
--getDirection a b c 
   -- | crsprd > 0  = Lt
   -- | crsprd == 0 = St
   -- | crsprd < 0  = Rt
   -- where crsprd =  (x b - x a) * (y c - y a) - (y b - y a) * (x c - x a)
   
-- Define a function that takes a list of 2D points and computes the direction 
-- of each successive triple. Given a list of points [a,b,c,d,e], it should 
-- begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], 
-- then [c,d,e]. Your function should return a list of Direction.   
-- getDirList :: [Cartesian2D] -> [Direction]
-- getDirList (a:b:c:xs) = (getDirection a b c) : getDirList (b:c:xs)
-- getDirList _          = []

-- Using the code from the preceding three exercises, implement Graham's scan 
-- algorithm for the convex hull of a set of 2D points. You can find good 
-- description of what a convex hull. is, and how the Graham scan algorithm 
-- should work, on Wikipedia.
