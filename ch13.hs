-- Real World Haskell
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

-- Necessary Imports

-- always use qualified import for Map as its functions conflict with those in
-- Prelude
import qualified Data.Map as Map

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

