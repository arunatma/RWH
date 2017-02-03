module PrettifyTest where 

-- this is part of ch11.hs
-- trying to test the Prettify library using property basaed quickcheck

import Test.QuickCheck
import Data.List
import Prettify

instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1, 6) :: Gen Int
        case n of
             1 -> return Empty

             2 -> do x <- arbitrary
                     return (Char x)

             3 -> do x <- arbitrary
                     return (Text x)

             4 -> return Line

             5 -> do x <- arbitrary
                     y <- arbitrary
                     return (Concat x y)

             6 -> do x <- arbitrary
                     y <- arbitrary
                     return (Union x y)


prop_empty_id x = 
    empty <> x == x
  &&
    x <> empty == x
    
docTest1 = quickCheck prop_empty_id

prop_char c   = char c   == Char c
prop_text s   = text s   == if null s then Empty else Text s
prop_line     = line     == Line
prop_double d = double d == text (show d)

docTest2 = quickCheck prop_char 
docTest3 = quickCheck prop_text 
docTest4 = quickCheck prop_line 
docTest5 = quickCheck prop_double 

-- fold (defined in Prettify) takes a function and a list of Doc, combines
-- using the function into a single Doc 
-- 'hcat' appends all Doc together - defined in Prettify, it is just fold (<>)
-- we shall do property testing for hcat
prop_hcat xs = hcat xs == glue xs
    where 
        glue []     = empty
        glue (d:ds) = d <> glue ds
        
docTest6 = quickCheck prop_hcat

-- property testing for punctuate function
-- punctuate is the same logic of 'intersperse' function from Data.List
prop_punctuate s xs = punctuate s xs == intersperse s xs

{-
    This is failing for multiple cases!
    
    *Main> quickCheck prop_punctuate
    *** Failed! Falsifiable (after 5 tests and 1 shrink):
    Union (Char '\144') (Union (Char '\226') Line)
    [Char '&',Char '\r']
    *Main> quickCheck prop_punctuate
    *** Failed! Falsifiable (after 5 tests and 1 shrink):
    Char '\DC4'
    [Char '{',Concat (Text "") (Text "\224\DEL")]
    *Main> quickCheck prop_punctuate
    *** Failed! Falsifiable (after 4 tests and 1 shrink):
    Empty
    [Empty,Line]    
    
-}

-- Reason:
-- Our Prettify optimises the redundant empty Doc, The model implementation 
-- (intersperse) doesn't replicate this scenario

-- Rewriting to take care of that scenario
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]
        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys

        