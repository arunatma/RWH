{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Real World Haskell
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

-- Necessary Imports

-- always use qualified import for Map as its functions conflict with those in
-- Prelude
import qualified Data.Map as Map
import DList
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable

-- Association Lists
-- Many instances need the use of key-value pairs
-- Association Lists and Data.Map help there
-- Map has considerable performance advantage over association lists
-- Association lists are simple and works like normal lists
-- This is just a list containing tuple of (key, value) pairs :: [(key, value)]

-- lookup (defined in Data.List) function: given a key, gets a value in Maybe
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
assocList = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
val1 = lookup 1 assocList       -- Just "one"
val2 = lookup 2 assocList       -- Just "two"

-- writing a lookup function
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup i ((k,v) : kvs)
    | i == k    = Just v
    | otherwise = myLookup i kvs 
myLookup i _    = Nothing

-- parsing an /etc/passwd file in a unix system
-- this file contains usernames, UIDs, home directories etc 
-- see passwd-al.hs

-- Data.Map provides map capability (dictionary / hash)
-- implemented internally as a balanced binary tree
-- My version here: 
-- https://github.com/arunatma/algorithmBST/blob/master/RedBlack.hs

-- building a map from association list
mapFromAL = Map.fromList assocList

-- map created using fold from assocList
mapFold = foldl (\map (k, v) -> Map.insert k v map) Map.empty assocList

-- Map using the manual insert
mapManual = Map.insert 2 "two" .
            Map.insert 4 "four" .
            Map.insert 1 "one" .
            Map.insert 3 "three" $ Map.empty
            
-- Map.insert 
-- Takes a map; and a new element; creates a copy; adds new element; gives back

-- Other functionalities in Map
-- Add or remove data 
-- Fold over them with a function 
-- convert to / fro association lists.

-- Functions being used as data
-- An example to appreciate better:
data CustomColor = CustomColor { red :: Int
                               , green :: Int
                               , blue :: Int } deriving (Eq, Show, Read)
     
-- generic function place holder; holds the name of function and the function     
data FuncRec = FuncRec { name :: String
                       , colorCalc :: Int -> (CustomColor, Int)}
                       
plus5func color x = (color, x + 5)

purple = CustomColor 255 0 255

plus5 = FuncRec { name = "plus5", colorCalc = plus5func purple }
always0 = FuncRec { name = "always0", colorCalc = \_ -> (purple, 0) }
                               
{-
    *Main> name plus5
    "plus5"
    *Main> :t colorCalc plus5
    colorCalc plus5 :: Int -> (CustomColor, Int)
    *Main> :t colorCalc always0
    colorCalc always0 :: Int -> (CustomColor, Int)
    
    *Main> (colorCalc plus5) 7
    (CustomColor {red = 255, green = 0, blue = 255},12)
    *Main> (colorCalc always0) 7
    (CustomColor {red = 255, green = 0, blue = 255},0)    
-}

-- plus5 and always0 functions internally have the data of purple color
-- FuncRec is abstract over plus5 and always0; does not contain any specific
-- colour data 

-- plus5 function has an internal string for name which is "plus5"
-- abstract that away and also the computation function which is in plus5func 
data FuncRec' = FuncRec' { name' :: String,
                           calc :: Int -> Int,
                           namedCalc :: Int -> (String, Int)}

mkFuncRec :: String -> (Int -> Int) -> FuncRec'
mkFuncRec name calcFunc = FuncRec' { name' = name,
                                    calc = calcFunc, 
                                    namedCalc = \x -> (name, calcFunc x)}
                                   
plus5' = mkFuncRec "plus5" (+ 5)
plus7  = mkFuncRec "plus7" (+ 7)
always0' = mkFuncRec "always0" (\_ -> 0)

{-  Exploring these functions:

    *Main> :t plus7
    plus7 :: FuncRec'
    *Main> :t name' plus7
    name' plus7 :: String
    *Main> :t calc plus7
    calc plus7 :: Int -> Int
    *Main> :t namedCalc plus7
    namedCalc plus7 :: Int -> (String, Int)
    *Main> name' plus7
    "plus7"
    *Main> (calc plus7) 8
    15
    *Main> (namedCalc plus7) 8
    ("plus7",15)
-}

-- Extended example using Map. See passwdmap.hs 

-- Numeric Types
-- Here is the need of the problem: To pretty show expressions
{-

    EXPRESSIONS PRINTING
    --------------------
    ghci> prettyShow $ 5 + 1 * 3
    "5+(1*3)"
    
    ghci> prettyShow $ 5 * 1 + 3
    "(5*1)+3"
    
    ghci> prettyShow $ simplify $ 5 + 1 * 3
    "5+3"
    
    REVERSE POLISH NOTATION
    -----------------------
    ghci> rpnShow $ 5 + 1 * 3
    "5 1 3 * +"
    
    ghci> rpnShow $ simplify $ 5 + 1 * 3
    "5 3 +"
    
    ALGEBRAIC EXPRESSIONS
    --------------------_
    ghci> prettyShow $ 5 + (Symbol "x") * 3
    "5+(x*3)"
    
    UNITS ARITHMETIC
    ----------------
    ghci> (units 5 "m") / (units 2 "s")
    2.5_m/s
    ghci> (units 5 "m") + (units 2 "s")
    *** Exception: Mis-matched units in add
    ghci> (units 5 "m") + (units 2 "m")
    7_m
    ghci> (units 5 "m") / 2
    2.5_m
    ghci> 10 * (units 5 "m") / (units 2 "s")
    25.0_m/s    
    
    LAZY EXPRESSION
    ---------------
    ghci> let test = 2 * 5 + 3
    ghci> test
    13
    ghci> rpnShow test
    "2 5 * 3 +"
    ghci> prettyShow test
    "(2*5)+3"
    ghci> test + 5
    18
    ghci> prettyShow (test + 5)
    "((2*5)+3)+5"
    ghci> rpnShow (test + 5)
    "2 5 * 3 + 5 +"
    
    TRIGONOMETRY
    ------------
    ghci> sin (pi / 2)
    1.0
    ghci> sin (units (pi / 2) "rad")
    1.0_1.0
    ghci> sin (units 90 "deg")
    1.0_1.0
    ghci> (units 50 "m") * sin (units 90 "deg")
    50.0_m
    
    OTHER ADVANCED FEATURES
    -----------------------
    ghci> ((units 50 "m") * sin (units 90 "deg")) :: Units (SymbolicManip Double)
    50.0*sin(((2.0*pi)*90.0)/360.0)_m
    ghci> prettyShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
    "50.0*sin(((2.0*pi)*90.0)/360.0)"
    ghci> rpnShow $ dropUnits $ (units 50 "m") * sin (units 90 "deg")
    "50.0 2.0 pi * 90.0 * 360.0 / sin *"
    ghci> (units (Symbol "x") "m") * sin (units 90 "deg")
    x*sin(((2.0*pi)*90.0)/360.0)_m
-}

-- All these are possible with the use of Haskell types and classes.
-- This is built in num.hs

-- We need to able to store expressions symbolically. The two operands can 
-- themselves be expressions.  So, it is kind of a tree structure
-- Before getting on to num.hs, some simple mechanisms here:
data Op = Plus | Minus | Mul | Div | Power
        deriving (Eq, Show)
        
-- Core symbolic manipulation type.
data SymbolicManip a =
          Number a
        | Arith Op (SymbolicManip a) (SymbolicManip a)
          deriving (Eq, Show)
          
-- This SymbolicManip will be an instance of Num
instance Num a => Num (SymbolicManip a) where
    a + b = Arith Plus a b
    a - b = Arith Minus a b
    a * b = Arith Mul a b
    negate a = Arith Mul (Number (-1)) a 
    abs a = error "abs is not implemented"
    signum _ = error "signum is not implemented"
    fromInteger i = Number (fromInteger i)
    
-- The instance puts a constraint on what 'a' could be.  It should be an 
-- instance of Num itself.  So, SymbolicManip Int is a proper type.

-- SymbolicManip can be a simple number; or an expression (as defined in 
-- SymbolicManip data type)

{-  A few trial runs:
    *Main> Number 5
    Number 5
    *Main> :t Number 5
    Number 5 :: Num a => SymbolicManip a
    *Main> :t Number (5::Int)
    Number (5::Int) :: SymbolicManip Int
    *Main> :t Number (5::Double)
    Number (5::Double) :: SymbolicManip Double
    *Main> Number 5 * Number 10
    Arith Mul (Number 5) (Number 10)
    *Main> (5 * 10) :: Int
    50
    *Main> (5 * 10) :: Double
    50.0
    *Main> (5 * 10) :: SymbolicManip Double
    Arith Mul (Number 5.0) (Number 10.0)
    *Main> (5 * 10) :: SymbolicManip Int
    Arith Mul (Number 5) (Number 10)
    *Main> (5 * 10 + 2) :: SymbolicManip Int
    Arith Plus (Arith Mul (Number 5) (Number 10)) (Number 2)
-}    

-- Now check num.hs for a detailed code

-- Taking advantage of functions as data
-- Appending lists in C
{-
    Just change the head and tail pointers to append or cut the list
    
    struct list {
        struct node *head, *tail;
    };
-}
-- No cost at all in C
    
-- Haskell - appending list
{-
    (++) :: [a] -> [a] -> [a]
    (x:xs) ++ ys = x : xs ++ ys
    _      ++ ys = ys
-}
-- Here the cost is directly proportional to the length of the list

-- (++) is infixr 5  (right assocative with priority 5)

x = "ab" ++ "bc" ++ "cd"       -- evaluated as "ab" ++ ("bc" ++ "cd")
-- but, if we incrementally append..
-- Total cost will be length of "bc" + length of "cd" = 4
y = "ab" ++ "bc"
z = y ++ "cd"
-- Here total cost is length of "bc" + length of "abbc" = 6 
-- becoming quadratic here - what should have been linear!

fx = ("ab" ++) . ("bc" ++)
gx = fx "cd"        -- same result, with linear time.
-- Taking advantage of the partial application and composition
-- The partial applications are not evaluated until required
-- But dealing with such partially applied functions is an awkward task.

-- Solution: Difference List
-- Look at DList.hs

-- Monoid: A structure in abstract algebra
-- Many math. objects are monoids
-- To be a monoid, only the following is to be satisfied
-- 1. An associative binary operator: a * (b * c) == (a * b) * c
-- 2. Presence of an identity element(e): a * e = e * a = a

-- No specification about the functionality of the binary operator 
-- The requirement is just that the binary operator should exist

-- (+) as binary operator and 0 as identity element, the integers form a monoid
-- (*) multiplication and 1 as identity element, the integers form a 2nd monoid

-- Monoids are everywhere in Haskell
-- Defined in Data.Monoid module 

{-

    class Monoid a where
        mempty  :: a                -- the identity
        mappend :: a -> a -> a      -- associative binary operator
    
    instance Monoid [a] where
        mempty  = []
        mappend = (++)
-}

-- we can define Monoid instance for DList - see DList.hs
-- DList.hs imported before using toList and fromList
mappendEx1 = toList (fromList [1,2] `mappend` fromList [3,4])

-- Although from a mathematical perspective, integers can be monoids in two 
-- different ways, we can't write two differing Monoid instances for Int in 
-- Haskell: the compiler would complain about duplicate instances

-- use of newtype to circumvent the same
newtype AInt = A { unA :: Int }
    deriving (Show, Eq, Num)
-- to derive Num class we need to add the following language pragma
-- Generalized Newtype Deriving
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}    

-- monoid under addition
instance Monoid AInt where
    mempty = 0
    mappend = (+)

newtype MInt = M { unM :: Int }
    deriving (Show, Eq, Num)

-- monoid under multiplication
instance Monoid MInt where
    mempty = 1
    mappend = (*)
    
mulAppend = 2 `mappend` 5 :: MInt           -- M 10
addAppend = 2 `mappend` 5 :: AInt           -- A 7

-- General purpose sequences
-- Data.Sequence module to be imported.

seq0 = Seq.empty                        -- fromList []
seq1 = Seq.singleton 5                  -- fromList [1]
seqn = Seq.fromList [1,2,3]             -- fromList [1,2,3]

seqComb1 = 1 Seq.<| Seq.singleton 2     -- fromList [1,2]

-- The operators are directly imported (see above)
seqComb2 = 1 <| Seq.singleton 2
seqComb3 = Seq.singleton 1 |> 2
seqComb4 = seqComb1 >< seqComb2         -- fromList [1,2,1,2]

-- creating list from Seq 
-- Use toList function from Data.Foldable
-- Done qualified import of Data.Foldable
list1 = Foldable.toList seqComb4        -- [1,2,1,2]

-- using fold function 
sum1 = Foldable.foldl' (+) 0 (Seq.fromList [1,2,3])
