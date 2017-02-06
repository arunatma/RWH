-- Real World Haskell
-- Chapter 12: Barcode recognition

-- Continuation of Chapter 10: Parsing Binary data
-- Two kinds of barcodes: UPC-A or EAN-13 (US or Europe)
-- EAN-13 developed later. It is a superset of UPC-A
-- EAN-13: 13 digit, in 4 groups
-- 1st Group: 2 digits - Number system (Nationality of Manuf. or ISBN)
-- 2nd Group: 5 digits - Manufacturer Id (assigned by the country)
-- 3rd Group: 5 digits - Product Id (assigned by manufacturer)
-- 4th Group: 1 digit - check digit - to validate the scan


import Data.Array (Array(..), (!), bounds, elems, indices,
                   ixmap, listArray)

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Parse                            -- from chapter 10


-- To calculate check digit:
-- Take the first 12 digits, multiply all even digits by 3. Add them together
-- Add the result with the sum of odd digits. Divide by 10. Find reminder.
-- Find the difference of 10 with the reminder. That's the check digit!
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)
    
mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

-- Sequence of bits in the barcode
-- Leading Guard Sequence: 101
-- A group of six digits, 7-bit wide
-- Another Guard Sequence: 01010
-- A group of six more digits, 7-bit wide
-- Trailing Guard Sequence: 101

-- Separate encodings for the six digits on the left and on the right
-- Parity bits encode the left group and the 13th digit.

-- Here, to get the random access in the list, we will be using "Array"
-- In bar code parsing, we use table look-ups
-- such table looks ups is difficult to achieve with lists or tuples
-- As list needs to be traversed linearly and tuple needs pattern matching

-- For constant time random access:  Array data structure
-- So, represent encoding tables as array of strings

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,l-1) xs
    where l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

-- Array has a special feature
-- One dimensional array of string will be of type Arry Int String
-- 2d array of string will be of type Array (Int, Int) String

-- array indices can be higher than the number of elements
-- But, if the list to be made array is of lesser length than array index,
-- exception thrown for the higher indices for which values from list not avbl
indexExceedsList = listArray (0,3) [True,False,False,True,False]
listExceedsIndex = listArray (0,10) "too short"
-- no error until the index 9 or 10 is accessed here
{-
    *Main> listExceedsIndex ! 8
    't'
    *Main> listExceedsIndex ! 9
    *** Exception: (Array.!): undefined array element
    *Main> listExceedsIndex ! 10
    *** Exception: (Array.!): undefined array element
    *Main> listExceedsIndex ! 11
    *** Exception: Ix{Integer}.index: Index (11) out of range ((0,10))
-}    
-- So, Arrays are lazy in evaluation

-- access the element using (!) operator
alphaArray = listArray (0,25) ['A'..]
letterZ = alphaArray ! 25
letterBeyond = alphaArray ! 100

-- the bounds need not start with 0
modulo7 = listArray (-3,3) [4,5..] 
reminder4 = modulo7 ! (-3)
reminder3 = modulo7 ! (3)

-- the bounds can be any member of 'Ix' type 
asciiVals = listArray ('A', 'Z') [65..]
asciiA = asciiVals ! 'A'
asciiZ = asciiVals ! 'Z'

-- multi dimensional array 
threeDimArray = listArray ((0,0,0), (9,9,9)) [0..]
elem654 = threeDimArray ! (6, 5, 4)

-- multi dimensional with multiple index types
twoDimArray = listArray ((0, 'a'), (9, 'z')) [0..]
elem9c = twoDimArray ! (9, 'c')

-- Folding over Arrays
-- bounds and indices function
-- bounds give the tuple used in creating the Array.
-- indices expands that tuple as a list.
{-
    *Main> bounds listExceedsIndex
    (0,10)
    *Main> indices listExceedsIndex
    [0,1,2,3,4,5,6,7,8,9,10]
-}

-- So, (range . bounds) === indices

-- No inbuilt fold functions in Data.Array module
-- Strict left fold similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
    where go s (j:js) = let s' = f s (a ! j)
                        in s' `seq` go s' (js)
          go s _ = s
          
-- Strict left fold (with first element of array as starting value)
-- similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

-- Why there is no default fold in Arrays?
-- fold is possible only in one dimension. On two dimension, row-wise and 
-- column-wise options come and there are multiple ways to think of a fold 
-- over n-dimensional data.

