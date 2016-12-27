-- http://book.realworldhaskell.org/read/using-typeclasses.html
-- Chapter 6: Using Typeclasses

import Data.Char (isSpace)
import SimpleJSON
import JSONClass

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
   
-- Typeclasses allow definition of generic interfaces
-- Typeclasses mandate certain functions to  be defined by any data type that 
-- use them.
-- Typeclasses also provide a default implementation of the functions and a 
-- data type instantiating the said typeclass can choose to override any of the
-- default implemented functions

-- ASSUME that "==" equality test do not exist in Haskell

-- To check for a custom data type 'Color', this is how the equality check has 
-- to be implemented
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

-- Now equality test for the strings
stringEq :: [Char] -> [Char] -> Bool

-- Match if both are empty
stringEq [] [] = True
-- If both start with the same char, check the rest
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
-- Everything else doesn't match
stringEq _ _ = False


-- Shortcoming here!
-- Need to use a function with a different name for every different type 
-- that needs equality testing.

-- Typeclasses to help
-- Typeclasses define a set of functions that can have different implementation
-- for different data types (same as what I wrot at the start of this page, 
-- just putting in different words)
-- Now, equality testing will have the same function (==) across the data types.

class BasicEq a where
    isEqual :: a -> a -> Bool
    
-- typeclass name is 'BasicEq'
-- works with any data type referred as 'a'
-- An instance of 'BasicEq' should define 'isEqual' function which takes two
-- arguments of the same data type and returns a 'Bool'

{-

*Main> :type isEqual
isEqual :: (BasicEq a) => a -> a -> Bool    

Here it says, isEqual is a function from BasicEq typeclass. Also signifies, 'a'
is a data type which is an instance of typeclass 'BasicEq'

-}

-- instance definition of 'Bool' for BasicEq.
-- Just need to define only one function 'isEqual' which 'BasicEq' mandates
instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False
    
    
{-
*Main> isEqual True False
False

*Main> isEqual 1 2
No instance for (BasicEq a0) arising from a use of `isEqual'

Works only for Bool, because we have defined it only for Bool
-}    
    
-- May be we can have a different class BasicEq2
-- This defines 2 functions
class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool
    
-- Another typeclass
-- Requiring 2 functions
-- Also, provides default implementations    
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)    
-- We just need to define at least one fn here (else it becomes a dead lock!)

{-

Actual Implementation in Prelude

class  Eq a  where
    (==), (/=) :: a -> a -> Bool

       -- Minimal complete definition:
       --     (==) or (/=)
    x /= y     =  not (x == y)
    x == y     =  not (x /= y)

-}


data Colors = Reds | Greens | Blues deriving (Eq, Show)
-- Now "Reds == Greens" kind of comparisons can be made.

instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False
    
-- Now "Red `isEqual3` Green" kind of comparisons can be made.
-- Because instance of BasicEq3 for Color is defined

instance BasicEq3 Char where
    isEqual3 x y = x == y
    
newtype Wrapper = Wrapper String deriving (Read, Show)
    
instance BasicEq3 Wrapper where
    isEqual3 (Wrapper []) (Wrapper [])          = True
    isEqual3 (Wrapper (x:xs)) (Wrapper (y:ys))  = 
        (isEqual3 x y) && (isEqual3 (Wrapper xs) (Wrapper ys))
    isEqual3 _ _                                = False


-- Built-in type classes

-- 'Show' 
---------

{-
    ghci> :type show
    show :: (Show a) => a -> String
-}

-- Has a function called "show" which takes any type and converts to String
-- show 1                   -- "1"
-- show "arun"              -- "\"arun\""
-- show [1,2]               -- "[1,2]"
-- show 'c'                 -- "'c'"
-- show "Hi, \"Jane\""      -- "\"Hi, \\\"Jane\\\"\""
-- See escaping and quotes are added by show function

-- ghci displays results as they would be entered into a Haskell program
-- To get the string that we want
-- putStrLn (show 'c')              -- 'c'
-- putStrLn (show "Hi, \"Jane\"")   -- "Hi, \"Jane\""


-- defining a show instance for "Color" data type
instance Show Color where
    show Red    = "Red"
    show Green  = "Green"
    show Blue   = "Blue"
    
-- 'Read'
---------

-- the counter class of 'Show' typeclass
{-
    ghci> :type read
    read :: (Read a) => String -> a
-}

doubleTheDouble = do
    putStrLn "Please enter a Double:"
    inpStr <- getLine
    let inpDouble = (read inpStr)::Double
    putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

-- Explicit type specification while "read" needed here because:
-- Many data types have instances for Read and Show. 
-- If there is no specific type mentioned, the compiler guesses one. 
-- Here it might choose Integer, which fails for a floating-point input.

-- Mostly, if the type specification is not there and compiler not able to 
-- guess, it throws an error.  Here the guess of "Integer" is because of (* 2)

{-

    ghci> (read "5")::Integer
    5
    ghci> (read "5")::Double
    5.0

    ghci> (read "5.0")::Integer
    *** Exception: Prelude.read: no parse    
    
    -- The Read class has some pretty complex parsers
    -- The instance chosen above is a different parser (Integer) which does 
    -- not expect "." in the number
-}

-- Writing a Read instance for Color data type
instance Read Color where
    readsPrec _ value = 
        -- We pass tryParse a list of pairs.  Each pair has a string
        -- and the desired return value.  tryParse will try to match
        -- the input to one of these strings.
        tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                      -- Compare the start of the string to be parsed to the
                      -- text we are looking for.
                      if (take (length attempt) (trim value)) == attempt
                         -- If we have a match, return the result and the
                         -- remaining input
                         then [(result, drop (length attempt) (trim value))]
                         -- If we don't have a match, try the next pair
                         -- in the list of attempts.
                         else tryParse xs


{-
    readsPrec makes it possible that the instance works for any higher data
    types as well!
    
    ghci> (read "Red")::Color
    Red
    ghci> (read "[Red]")::[Color]
    [Red]
    ghci> (read "[Red,Red,Blue]")::[Color]
    [Red,Red,Blue]
    ghci> (read "[Red, Red, Blue]")::[Color]
    *** Exception: Prelude.read: no parse

    -- The leading white spaces are not handled by the parser defined above!
    
    -- Defined now: Using "trim" to remove the leading or trailing white space.
-} 

-- Note:
-- Read is not widely used.
-- Many people use "Parsec" for writing the parsers 

-- Serialization with Read and Show
-- serialization: The process of converting the data structure in memory into 
-- serialized bits to be stored on to disk
-- show and read: also function as tools for serialization
-- show: output is both human readable and machine-readable.

-- writing
d1 = [Just 5, Nothing, Nothing, Just 8, Just 9]::[Maybe Int]
d1print = putStrLn (show d1)      -- [Just 5, Nothing, Nothing, Just 8, Just 9]
d1write = writeFile "test.txt" (show d1) -- Creates test.txt when triggered

-- reading
d2read = readFile "test.txt"

d2 = do
    d2readstr <- d2read
    return((read d2readstr) :: [Maybe Int])

{-
-- Numeric Types

Double   Double-precision floating point. A common choice for floating-point.
Float    Single-precision floating point. Often used when interfacing with C.
Int      Fixed-precision signed integer;  range [-2^29..2^29-1]. Commonly used.
Int8     8-bit signed integer
Int16    16-bit signed integer
Int32    32-bit signed integer
Int64    64-bit signed integer
Integer  Arbitrary-precision signed integer; range limited only by machine 
         resources. Commonly used.
Rational Arbitrary-precision rational numbers. Stored as a ratio of two Integers
Word     Fixed-precision unsigned integer; storage size same as Int
Word8    8-bit unsigned integer
Word16   16-bit unsigned integer
Word32   32-bit unsigned integer
Word64   64-bit unsigned integer


Int8, Int16, Int32, Int64 are defined in Data.Int
Word8, Word16, Word32, Word64 are defined in Data.Word

-}

{-
-- Numeric TypeClasses

    Bits
    Bounded
    Floating
    Fractional
    Integral
    Num
    Real
    RealFrac

-}

{-
-- Which typeclass instances are defined for which types?
Type    Bits    Bounded    Floating    Fractional    Integral    Num  Real   RealFrac
Double                     X           X                         X    X      X
Float                      X           X                         X    X      X
Int      X        X                                  X           X    X
Int16    X        X                                  X           X    X
Int32    X        X                                  X           X    X
Int64    X        X                                  X           X    X
Integer  X                                           X           X    X
Rational or any Ratio                  X                         X    X      X
Word     X        X                                  X           X    X
Word16   X        X                                  X           X    X
Word32   X        X                                  X           X    X
Word64   X        X                                  X           X    X
-}

{-
-- Conversion from one type to another - which function to use?

Source Type                                    Destination Type
                    Double, Float               Int, Word       Integer         Rational
Double, Float       fromRational . toRational   truncate*       truncate*       toRational
Int, Word           fromIntegral                fromIntegral    fromIntegral    fromIntegral
Integer             fromIntegral                fromIntegral    N/A             fromIntegral
Rational            fromRational                truncate*       truncate*       N/A

truncate* = any of the functions: truncate, ceiling, floor, round
-}

-- Equality, Ordering and Comparisons
-- Most of the data types can derive the Eq and Ord type classes so that they 
-- can be compared against each other or ordered.

data SimpleDt = PointA | PointB | PointC deriving (Eq, Ord, Read, Show)
-- Simple data types can automatically derive the above typeclasses.
-- A complex one (like one involving a function) cannot!

data MyType = MyType (Int -> Bool) 

-- working with JSON
-- In JSON, a list can have any values
-- But in our implementation, there is a constraint: all should be of same type
result :: JValue
result = JObject [
  ("query", JString "awkward squad haskell"),
  ("estimatedCount", JNumber 3920),
  ("moreResults", JBool True),
  ("results", JArray [
     JObject [
      ("title", JString "Simon Peyton Jones: papers"),
      ("snippet", JString "Tackling the awkward ..."),
      ("url", JString "http://.../marktoberdorf/")
     ]])
  ]

-- See JSONClass.hs  

-- newtype keyword
data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)
-- NewtypeInt can be checked for equality, ordered or printed
-- It cannot use the other typeclasses of "Int" (like Num, Real etc)
    
newtype UniqueID = UniqueID Int
    deriving (Eq)
    
-- newtype and type though look similar, are not the same.
-- type synonym is interchangeable - [Char] with String
-- But the newtype is not interchangeable
    
-- Most important:
-- a "newtype" can have only one data constructor
-- that data constructor must exactly have one field (no less no more)
    
-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- ok: exactly one field
newtype Okay = ExactlyOne Int

-- ok: type parameters are no problem
newtype Param a b = Param (Either a b)

-- ok: record syntax is fine
newtype Record = Record {
      getInt :: Int
    }

{-    
-- bad: no fields
newtype TooFew = TooFew

-- bad: more than one field
newtype TooManyFields = Fields Int Int

-- bad: more than one constructor
newtype TooManyConstrs = Bad Int
                       | Worse Int    
-}

-- Extra overhead needed for processing - to understand using which data 
-- constructor the value is created (when using data keyword)
-- No such overhead in newtype
-- Space and Time efficient in run time.

-- When we apply the N constructor in an expression (or in a pattern), we 
-- coerce an expression from type Int to type NewtypeInt. No overhead in runtime

-- JSON typeclasses without overlapping instances 
