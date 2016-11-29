{- # OPTIONS_GHC -fwarn-name-shadowing # -}

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

-- an instance of BookInfo 
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]


-- The type constructor and value constructor can be of the same name. Haskell
-- identifies it differently and there is not confusion - because the context
-- with which a value constructor or a type constructor are used will be 
-- different.
data BookReview = BookReview BookInfo CustomerID String         
-- Because they are used in a different context, no ambiguity
-- Try ":t BookReview" in ghci
-- BookReview :: BookInfo -> CustomerID -> String -> BookReview
-- In the above type signature, the first BookReview refers to the value
-- constructor (the function that is used to create a new value), and the final
-- BookReview refers to the type of the value that just got created

-- Type Synonym is just a new name for an existing (also user created) data type
-- A type synonym can be interchanged with the type it represents. It is just
-- a better descriptive name 
-- Int and CustomerID can be used interchangeably
type CustomerID = Int               -- creating a type synonymn
type ReviewBody = String
-- Using the type synonym to create "BetterReview"
data BetterReview = BetterReview BookInfo CustomerID ReviewBody

myReview = BetterReview myInfo 5 "Review is here"

-- see that the return type is Int (can also be CustomerID - no distinction)
getCustomerId :: BetterReview -> Int
getCustomerId (BetterReview x y z) = y

custId = getCustomerId myReview     -- 5

-- Creating a type synonym for a tuple, to be used easily in code.
type BookRecord = (BookInfo, BookReview)

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
-- both x and y have same types here, which can be distinguished for 
-- readability and for avoid mistreating one to be another

data CricketGround = CricketGround String String
data SpaceProbe = SpaceProbe String String


-- Algebraic data types help to identify specific piece of data, which might
-- be otherwise undistinguishable
a = ("Toyota", "Innova")
b = ("Table", "Oak")

data Car = Car String String
data Furniture = Furniture String String

c = Car "Toyota" "Innova"
d = Furniture "Table" "Oak"

-- In c and d, we know the two strings make up for Car data type where as in 
-- d, these two strings make up for Furniture data type 
-- a and b are of equivalent types, whereas c and d are not, so the chance of 
-- accidentally mixing up these do not ever arise.
-- Finer Example, here
-- data types can be thought of as similar to Structure in C language

-- x and y lengths
data Cartesian2D = Cartesian2D Double Double deriving (Eq, Show)
-- distance from origin and angle
data Polar2D = Polar2D Double Double deriving (Eq, Show)

-- deriving (Eq) let us compare the values for equality, it derives the 
-- properties of typeclass Eq
-- A tuple (1,2) may represent Cartesian as well as a Polar coordinate
-- At any point in time, a Cartesian coordinate not to be compared with a polar
-- coordinate or substituted for one another. Algebraic Data Type helps here


-- Cartesian to Polar Conversion
polar :: Cartesian2D -> Polar2D
polar (Cartesian2D x y) = Polar2D distance theta
    where distance = sqrt (x^2 + y^2)
          theta    = atan (y/x)
          
-- Polar to Cartesian Conversion          
cartesian :: Polar2D -> Cartesian2D
cartesian (Polar2D r t) = Cartesian2D x y 
    where x = r * cos (t)
          y = r * sin (t)
          
-- The following expression throws up a type error
-- Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2

-- An alternate set of data types
data Cartesian2D' = Cartesian Double Double 
                    | PolarToCartesian Polar2D'
                    deriving (Eq, Show)

data Polar2D' = Polar Double Double
                | CartesianToPolar Cartesian2D'
                deriving (Eq, Show)

{-
Comparing the Haskell data type with C "struct":
------------------------------------------------
"C" Language struct definition for book_info
struct book_info {
   int id;
   char *name;
   char **authors;
};

Haskell definition:
data BookInfo = Book Int String [String]
                deriving (Show)
             
-- The fields in a algebraic data type are anonymous and positional
-- anonymous - means field themselves do not have any name
-- positional - since it has no name, it needs to be referred by the position           
-- Btw, using record syntax, we can do away with the "positional" restriction

-}
-- Algebraic data type are used to create data type similar to enumeration in C
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




-- Pattern Matching
-- 1. When a type has more than 1 value constructor, we can identify which 
-- value constructor was used to create a value 
-- 2. If a value constructor has data, to be able to extract data.

-- Not two functions, the same function "myNot" defined as series of expressions
-- True and False here are value constructors, depending on which one was used
-- to construct the value, appropriate expression for "myNot" is selected
myNot True = False
myNot False = True

-- another example of pattern matching
sumList (x:xs) = x + sumList xs
sumList []     = 0
-- The way it executes is 
-- sumList [1,2] = sumList (1:(2:[]))
--               = 1 + sumList (2:[])
--               = 1 + 2 + sumList []
--               = 1 + 2 + 0
--               = 3

-- pattern matching acts as inverse of the type construction, getting the values
-- out of the components, so sometimes called "deconstruction"

-- Coming back to the book store example

-- Overlap between tuple and algebraic data types                    
myBookInfo = Book 2 "The Wealth of Networks" ["Yochai Benkler"]
myBookTuple = (2,"The Wealth of Networks",["Yochai Benkler"])  

-- Taking a value of Book or Author or Title from the BookInfo 
-- Using Pattern matching
bookId      (Book id title authors) = id
bookTitle   (Book id title authors) = title 
bookAuthors (Book id title authors) = authors

-- we do not care about other entities when we look for id.
nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors

-- The above ways of defining separate function for each field is cumbersome
-- There is an automatic way of defining those functions, using "Record Syntax"

data Customer = Customer{
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

-- Here customerID, customerName and customerAddress are pre-defined functions 
-- to extract the specific values.

-- the usual implementation without record syntax 
-- code is commented because it uses the same names which are already defined.

{-

data Customer = Customer Int String [String]
                deriving (Show)

customerID :: Customer -> Int
customerID (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address

-}

-- btw, the record syntax are not needed  while creating a value
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

-- but, can be used to make the code more readable
-- and to change the order of the fields
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }             

-- another example, defined in System.time
type Month = Int
type Day = String
data CalendarTime = CalendarTime {
  ctYear                      :: Int,
  ctMonth                     :: Month,
  ctDay, ctHour, ctMin, ctSec :: Int,
  ctPicosec                   :: Integer,
  ctWDay                      :: Day,
  ctYDay                      :: Int,
  ctTZName                    :: String,
  ctTZ                        :: Int,
  ctIsDST                     :: Bool
}

----------------------
-- Parameterised types
----------------------

-- Maybe is already defined in Prelude
data Maybe' a = Just' a | Nothing' deriving (Show)
-- Here 'a' is the type variable. It can be Int, Char, [Char], [[Int]] anything!
-- Maybe is a parameterised type; it takes another type as parameter.

-- Maybe is polymorphic, where as Maybe Int or Maybe [Bool] is a distinct type.
-- It may as well be Maybe (Maybe [Char])
wrapped = Just (Just "wrapped")

-- Parameterised types are similar to "templates" in C++ or "generics" in Java

------------------
-- Recursive types 
------------------
-- A type defined in terms of itself.
data List a = Cons a (List a) 
            | Nil
              deriving (Show)
              
-- List has a type variable 'a' and has two type constructor "Cons" and "Nil"

-- Cons as a data or value constructor takes two inputs:
-- a value of type 'a' and another of type (List a) and gives (List a)

-- List appears both on left and right of "=" sign, a type referring itself.

nilList = Nil 
valList1 = Cons 0 Nil
valList2 = Cons 1 (Cons 0 Nil)

-- The List data type defined above is equivalent and identical to the built
-- in list [].  List and [] are not identical actually, they are "isomorphic" 
-- having the same shape
fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList :: List a -> [a]
toList (Cons x xs) = x:(toList xs)
toList Nil         = []
               
-- Recursive Binary Tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
              
{-

An equivalent of the same in Java using generics.

class Tree<A>
{
    A value;
    Tree<A> left;
    Tree<A> right;

    public Tree(A v, Tree<A> l, Tree<A> r)
    {
        value = v;
        left = l;
        right = r;
    }
}

-- Java lets us use "null" in constructing trees, when there is no sub tree.

class Example 
{
    static Tree<String> simpleTree()
    {
        return new Tree<String>(
            "parent",
            new Tree<String>("left leaf", null, null),
            new Tree<String>("right leaf", null, null));
    }
}

-}              
  
-- We can create the same tree with two leaves, in Haskell using the Empty 
-- data constructor that we defined for Tree 
-- null in Java works anywhere, "Empty" is a data constructor specific to "Tree"

simpleTree = Node "simple tree" (Node "left leaf" Empty Empty)
                           (Node "right leaf" Empty Empty)  
  

-- Defining Tree type with just one constructor (similar to Java)
-- Using Maybe type instead of "Empty" data constructor.

{-
Todo:  Make such a tree and make fromTree and toTree functions.
Currently the following code do not work.

data Tree' a = Maybe (Node' a (Tree' a) (Tree' a)) deriving (Show)

simpleTree' = Just (Node' "simple tree" (Just (Node' "left" Nothing Nothing))
                            (Just (Node' "right" Nothing Nothing)))
                            
-- Tree to Tree' conversion                            
fromTree :: Tree a -> Tree' a
fromTree (Node a l r)   = Just (Node' a (fromTree l) (fromTree r))
fromTree Empty          = Nothing        
-}


-- Reporting Errors

-- :t error 
-- error :: [Char] -> a 
-- The idea that the return type can be "any type" is that you can call it 
-- anywhere and it will always have the right type

-- getting the second character in the list 
mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "List is too short"
              else head (tail xs)
              
-- Disadvantages of using "error"
-- You will not be allowed to recover and the program is terminated

-- Controlled Approach
-- Use "Maybe"              

safeSecond :: [a] -> Maybe a
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))


-- Improving the readability with pattern matching
tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x   -- Pattern matches the list with at least 2 chars
tidySecond _       = Nothing  -- This matches the rest


-- Local Variables
-- usage of "let .. in"
lend amount balance = let reserve = 100 
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance
               
-- whatever follows "in" is the expression that will be evaluated and will be 
-- treated as the value of the expression.
-- the "in" blocks uses some variables which are defined in the "let" block

-- Lazy evaluation:
-- The names in the "let" block are assigned expressions which are evaluated 
-- only when in need. i.e. the "in" block getting called, which uses them.
-- Example: If balance is less than reserve, "newBalance" will not be computed.

-- Names assigned in a "let" block can be used only in the following "in" block

-- Nesting of "let .. in" blocks 
foo = let a = 1
      in let b = 2
         in a + b

{- The way the expression unfolds
foo = let b = 2 
      in a + b 

foo = a + b

foo = a + 2

foo = let a = 1 
      in a + 2 
      
foo = 1 + 2

foo = 3      
       
-}       

-- Using the same names in nested let
-- The inner "x" hides / shadows the outer "x" ("shadowing")
bar = let x = 1
    in ((let x = "foo" in x), x)
{-
bar = ((let x = "foo" in x), x)

bar = ("foo", x)

bar = let x = 1
      in ("foo", x)
      
bar = ("foo", 1)      

-}

-- Shadowing function parameters!
quux a = let a = "foo"
         in a ++ "eek!"
         
-- :t quux 
-- quux :: t -> [Char]
-- The input is ignored (so, can be of any type), return value is "fooeek!"
-- quux [[3], []] = "fooeek!"
-- quux 1 = "fooeek!"
-- quux 'a' = "fooeek!"

-- Use "fwarn-name-shadowing" to detect these while compiling. Used at top
-- Can also be enabled with *Main> :set -Wall in GHCI 

-- usage of "where"
-- "where" also helps in defining the local variables.
-- the names in "where" block apply to the code that precedes it 
-- Same lending example using "where" 
lend2 amount balance = if balance < reserve
                       then Nothing
                       else Just newBalance
    where reserve = 100 
          newBalance = balance - amount

-- Local functions
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"
-- Here, "plural" is a local function whose scope is within the "pluralise" fn.
-- See also "word" whose scope is throughout the "pluralise" function.
-- Similar to functions, the variables defined at the top of the source file
-- becomes a global variable 


          
          
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
--sortLen xs ys = compare (length xs) (length ys)
--sortListLen = sortBy sortLen

-- Define a function that joins a list of lists together using a separator value
-- intersperse' ',' ["foo","bar","baz","quux"] == "foo,bar,baz,quux"
intersperse' :: a -> [[a]] -> [a]
intersperse' x [] = []
intersperse' x (y:ys) = if null ys 
                        then y 
                        else y ++ ([x] ++ intersperse' x ys)

intersperse'' :: a -> [[a]] -> [a]
intersperse'' x ys = intercalate [x] ys


-- Using the binary tree type that we defined earlier in this chapter, write a 
-- function that will determine the height of the tree. The height is the 
-- largest number of hops from the root to an Empty. For example, the tree 
-- Empty has height zero; Node "x" Empty Empty has height one; 
-- Node "x" Empty (Node "y" Empty Empty) has height two; and so on
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


