-- Real World Haskell
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html



--------------------------------------------------------------------------------
-- Imports
import qualified Data.Map as Map
--------------------------------------------------------------------------------

-- Association List
-- A list containing the key - value tuples
asscnList = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

-- lookup function
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- In a list containing (a, b) tuples, given a, if present, output b
value1 = lookup 1 asscnList     -- Just "one"
value2 = lookup 2 asscnList     -- Just "two"
value5 = lookup 5 asscnList     -- Nothing

-- Writing our own lookup function
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' a (x:xs) = 
    if a == (fst x) 
        then Just (snd x) 
        else lookup' a xs

-- See 13PasswdAL.hs for a detailed example (covered in this chapter in book)

-- Data.Map
-- Provides functions to work with Association Lists - much better performance
-- Implemented as a balanced binary tree, internally
-- Some functions in Data.Map have same names as those in Prelude; So use
-- a qualified import
-- (given at the start of the file)

-- Three ways of creating a map.
-- 1. creating a map from association list
mapFromAL = Map.fromList asscnList

-- 2. Using foldl and empty map
mapFold = foldl (\map (k, v) -> Map.insert k v map) Map.empty asscnList

-- 3. Manual creation
mapManual = 
    Map.insert 2 "two" .
    Map.insert 4 "four" .
    Map.insert 1 "one" .
    Map.insert 3 "three" $ Map.empty
    

-- Functions are data, too!!
-- Define a custom type
data CustomColor = CustomColor {red :: Int, green :: Int, blue :: Int}
        deriving (Eq, Show, Read)

-- Another data type that stores a name and a function
data FuncRec = FuncRec {name :: String, 
                        colorCalc :: Int -> (CustomColor, Int)}

-- one more function
plus5Func color x = (color, x + 5)

-- define a color using CustomColor data type
purple = CustomColor 255 0 255                        

plus5 = FuncRec {name = "plus5", colorCalc = plus5Func purple}
always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}

-- define red color
pureRed = CustomColor 255 0 0
plus5Red = FuncRec {name = "plus5", colorCalc = plus5Func pureRed}

-- FuncRec actually acts as a "closure"
-- Does not have a separate field to store "Color" but captures using colorCalc

-- Making data available in multiple places
-- Using Type Construction Functions

data FuncRec' = FuncRec' {name' :: String,
                          calc' :: Int -> Int, 
                          namedCalc' :: Int -> (String, Int)}

mkFuncRec :: String -> (Int -> Int) -> FuncRec'
mkFuncRec name calcFunc = FuncRec' {name' = name,
                                    calc' = calcFunc,
                                    namedCalc' = \x -> (name, calcFunc x)}

plus5' = mkFuncRec "plus5" ( + 5)
always0' = mkFuncRec "always0" (\_ -> 0)

--------------------------------------------------------------------------------
-- In GHCI -- 
{-
ghci> :t plus5'
plus5' :: FuncRec'
ghci> name' plus5'
"plus5"
ghci> (calc' plus5') 5
10
ghci> (namedCalc' plus5') 5
("plus5",10)
ghci> let plus5a = plus5' {name' = "PLUS5A"}
(now both plus5' and plus5a exist, plus5a is another container completely)

ghci> name' plus5a
"PLUS5A"
ghci> (namedCalc' plus5a) 5
("plus5",10)
-}
--------------------------------------------------------------------------------

-- A detailed example of association list using Map 
-- Extension of 13PasswdAL.hs
-- Have a look at 13PasswdMap.hs

--------------------------------------------------------------------------------

-- Extended Example: Numeric Types
-- Playing around with custom number types
-- With options to represent expressions, simplify expressions, including units
-- Enabling Reverse Polish Notation

-- FIRST ATTEMPT
-- A Simpler Version

-- Operators supported 
data Op = Plus | Minus | Mul | Div | Pow deriving (Eq, Show)

-- SymbolicManip data type definition
data SymbolicManip a = Number a | Arith Op (SymbolicManip a) (SymbolicManip a)
    deriving (Eq, Show)
-- (note above, that the Arith data type constructor is recursive)    
    
-- SymbolicManip is an instance of Num
-- Num mandates certain functions are to be defined for its instances

instance Num a => Num (SymbolicManip a) where
    a + b = Arith Plus a b
    a - b = Arith Minus a b 
    a * b = Arith Mul a b 
    negate a        = Arith Mul (Number (-1)) a
    abs a           = error "abs is not implemented"
    signum _        = error "signum is not implemented"
    fromInteger i   = Number (fromInteger i)
    
--------------------------------------------------------------------------------
{-
ghci> :l 13numsimple.hs
[1 of 1] Compiling Main             ( numsimple.hs, interpreted )
Ok, modules loaded: Main.
ghci> Number 5
Number 5
ghci> :t Number 5
Number 5 :: (Num t) => SymbolicManip t
ghci> :t Number (5::Int)
Number (5::Int) :: SymbolicManip Int
ghci> Number 5 * Number 10
Arith Mul (Number 5) (Number 10)
ghci> (5 * 10)::SymbolicManip Int
Arith Mul (Number 5) (Number 10)
ghci> (5 * 10 + 2)::SymbolicManip Int
Arith Plus (Arith Mul (Number 5) (Number 10)) (Number 2)
-}
--------------------------------------------------------------------------------
    
-- Extension of the above example is given in 13num.hs
-- which is a detailed haskell file, having advanced features implemented.
             

    