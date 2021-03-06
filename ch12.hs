-- Real World Haskell
-- Chapter 12: Barcode recognition
-- http://book.realworldhaskell.org/read/barcode-recognition.html

-- Continuation of Chapter 10: Parsing Binary data
-- Two kinds of barcodes: UPC-A or EAN-13 (US or Europe)
-- EAN-13 developed later. It is a superset of UPC-A
-- EAN-13: 13 digit, in 4 groups
-- 1st Group: 2 digits - Number system (Nationality of Manuf. or ISBN)
-- 2nd Group: 5 digits - Manufacturer Id (assigned by the country)
-- 3rd Group: 5 digits - Product Id (assigned by manufacturer)
-- 4th Group: 1 digit - check digit - to validate the scan


import Data.Array (Array(..), (!), bounds, elems, indices,
                   ixmap, listArray, accum)

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import Data.Function (on)
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

-- Modifying Array elements
-- arrays are immutable, modification means recreating the entire array with 
-- the change
-- 'accum' functions takes an array and a list of (index, value) pairs to 
-- change it to the 'value' at the 'index' in the array.
-- accum  :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e
-- So, this kind of modification is extremely costly

-- Exercise
-- 1. Write a function that takes two arguments: a four-element tuple, andd an 
-- integer. The integer is to represent the position of the element inside the
-- tuple. 0 gets the first element, 1 the second element and so on
data TupleFour a b c d = First a | Second b | Third c | Fourth d deriving (Show)
takeFromTuple :: (a, b, c, d) -> Int -> TupleFour a b c d
takeFromTuple (x, y, z, w) k 
    | k == 0 = First x
    | k == 1 = Second y
    | k == 2 = Third z
    | k == 3 = Fourth w
    | otherwise = error "Out of Bounds"
    
-- Back to the goal of this chapter: Decoding the bar code
-- Better to have encoder for reference. Useful to cross check.

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) = 
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])
        
leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard = "101"
centerGuard = "01010"

-- string to encode: 12 digits long.  Encoder addes the 13th checkDigit 
-- Encoded as group of two 6-digits.

-- Each digit in the left group is encoded using either odd or even parity, 
-- The parity is chosen based on the bits of the first digit in the string. 
-- If a bit of the first digit is zero ==> encoded with even parity
-- If one ==> encoded with odd parity
-- This helps in maintaining the compatibility with UPC-A standard.

-- Constraints on the decoder:
-- Camera images are generally stored in JPEG format
-- JPEG decoder has its own standard.
-- So, instead of that, we use netpbm file described in Ch 10.

-- Objective: Take a camera image and extract a valid barcode.
-- How?
-- Convert color data into an easily workable format
-- Sample a single scan line and guess the info
-- From the guesses, create a list of valid decodings.

-- Processing a netpbm color image (we did grayscale image in ch10)
-- Identifying string is "P6" here as opposed to "P5" in grayscale
-- Rest of the header is identical to the grayscale format.
-- In the body: Each pixel is represented by 3 bytes (Red, Green and Blue)

-- Data structure for the image:
-- 2-Dimensional array of images.
type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)

type Pixmap = Array (Int, Int) RGB

-- indices start at zero
-- also we are not storing the dimensions of the image explicitly (can be got
-- using the "bounds" function of the array)

parseRawPPM :: Parse Pixmap
parseRawPPM = 
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P6") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxValue ->
    assert (maxValue == 255) "max value out of spec" ==>&
    parseByte ==>&
    parseTimes (width * height) parseRGB ==> \pxs ->
    identity (listArray ((0,0),(width-1, height-1)) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte ==> \r ->
           parseByte ==> \g ->
           parseByte ==> \b ->
           identity (r, g, b)

-- parseTimes calls another parser for a given number of times, building up a 
-- list of results           
parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x:) <$> parseTimes (n - 1) p
           
-- Grayscale conversion
luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r, g, b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
    where r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b

type Graymap = Array (Int, Int) Pixel

pixmapToGraymap :: Pixmap -> Graymap
pixmapToGraymap = fmap luminance

-- greyscale image to binary - To see whether the pixel is on or off.
-- We can use Pixel for representing 1 or 0
-- But this may lead to some cases where we mistakenly give bit for pixel or 
-- pixel for bit and compiler cannot catch.
-- Even a type synonmym cannot help - as compiler treats both equally
-- So, use a different data type to help ourselves restricting the mixed use.

data Bit = Zero | One deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
    where binary i | i < pivot = Zero
                   | otherwise = One
          pivot    = round $ least + (greatest - least) * n
          least    = fromIntegral $ choose (<) a
          greatest = fromIntegral $ choose (>) a
          choose f = foldA1 $ \x y -> if f x y then x else y
          
-- So, what all has been done till now?
-- 1. Converted from colour to monochrome
-- 2. Made a threshold of 1 or 0 based on the value of the pixel.

-- On a barcode scan, if we had to encode 9 780132 114677, Left group encodes
-- 780132 and right group encodes 114677.  9 is encoded in the parity and the 
-- last 7 is the check digit.

-- The bar code image captured has to be adjusted for contrast, then applied
-- threshold so that we get a better sense of the lines
-- The decoding code must be robust because in some cases the bars are too 
-- thick, too thin, or not exactly where they're supposed to be. The widths of 
-- the bars will depend on distance from camera.  So, no assumptions can be 
-- made on the widths.

-- How to approach?
-- Find the digits that "might" be encoded at a given position.
-- Assumptions:
-- 1. We are working with a single row of barcode image
-- 2. We know the left edge of the bar code (we know where the barcode begins)

-- How to overcome the width assumption (not knowing what width each bar is)
-- Run Length Encoding!

type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group 
    where rle xs = (length xs, head xs)
    
-- new function "group" here
-- group :: Eq a => [a] -> [[a]]
-- puts sequential identical elements in sub lists
-- group [1,2,2,3,5,3,4,5,1] == [[1],[2,2],[3],[5],[3],[4],[5],[1]]

bits = [0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0]
rleBits = runLength bits
-- [(2,0),(2,1),(2,0),(2,1),(6,0),(4,1),(4,0)]

stateName = "Mississippi"
rleState = runLength stateName
-- [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]

-- throw away the element (since it will always be 1 or 0)
-- for a list of strings, get the runs alone
runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

-- Scaling run lengths and finding approximate matches
-- scale the run lengths, so they sum up to 1
type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
    where divide d = fromIntegral d / divisor
          divisor = fromIntegral (sum xs)
-- A more compact alternative that "knows" we're using Ratio Int:
-- scaleToOne xs = map (% sum xs) xs

type ScoreTable = [[Score]]

-- SRL: Scored Run Length
asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

-- Advantage of 'Score' type synonym
-- We do not expose the type to other functions, later if we want to change
-- it to Double, instead of Ratio Int, it is much easier to do

-- Comparing two 'Score's using the distance calculation
-- Zero distance means exact match and large differences means the Scores do 
-- not match - or the Scores do not represent the same patterns

distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

sampleRun = scaleToOne [2,6,4,4]
dist1 = distance sampleRun (head leftEvenSRL)        -- 13 % 28
dist2 = distance sampleRun (head leftOddSRL)        -- 17 % 28

-- Given a scaled run length table, we need to choose the best few matches
-- in the table for the given input
bestScores :: ScoreTable -> [Run] -> [(Score, Int)]
bestScores srl ps = take 3. sort $ scores
    where scores = zip [distance d (scaleToOne ps) | d <- srl] digits
          digits = [0..9]
          
-- Introduction of list comprehension
-- Happened to use above:
-- [distance d (scaleToOne ps) | d <- srl] 
listComp1 = [ (a,b) | a <- [1,2], b <- "abc" ]        
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
-- Expression on the left side of vertical bar (|) is evaluated for each 
-- combination of "generator expression" on the right
-- "Depth First" order. For the first element of first list, every element of 
-- second list is evaluated.

-- This is similar to a set expression in linear algebra.
-- (x, y) such that x belongs to N, y belongs to W, x is odd and y is even
-- and both x and y < 10 
listComp2 = [(x,y) | x <- [1,2..9], y <- [0,1..9], odd x, even y]
-- "odd x" here is a guard.  x <- [1,2..9] is a generator
-- Guard is of type "Bool", if False, the element is skipped

-- We can use guard for an expression using generator variables
listComp3 = [ (a,b) | a <- [1..6], b <- [5..7], even (a + b ^ 2) ]

-- Binding local variables within generator expressions using "let"
-- join characters when both are vowels
vowel = (`elem` "aeiou")
listComp4 =  [ x | a <- "erlang", b <- "clojure", let x = [a,b], all vowel x ]
-- ["eo","eu","ee","ao","au","ae"]

-- pattern match in generator expression
-- failing a match causes no error - just that the element is skipped
checkList = [(1,'y'),(3,'e'),(5,'p'), (3,'z')]
listComp5 = [ a | (3,a) <-  checkList]            -- "ez"
-- picking out all items, which has 3 as the first element.

-- Multiple version of "bestScores" function
{-
    -- our original
    zip [distance d (scaleToOne ps) | d <- srl] digits

    -- the same expression, expressed without a list comprehension
    zip (map (flip distance (scaleToOne ps)) srl) digits

    -- the same expression, written entirely as a list comprehension
    [(distance d (scaleToOne ps), n) | d <- srl, n <- digits]
-}

-- Coming back to bar code recognition
-- For each match in left group, we need to remember whether the match is found
-- in even parity or odd parity table
data Parity a = Even a | Odd a | None a deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
    fmap = parityMap
    
-- sort parity encoded values based on the values.
-- using "on" function from Data.Function
{-
    on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
    on f g x y = g x `f` g y
-}
compareWithoutParity :: Ord a => (Parity a) -> (Parity a) -> Ordering
compareWithoutParity = compare `on` fromParity

type Digit = Int

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity 
              ((map Odd (bestScores leftOddSRL ps)) ++
               (map Even (bestScores leftEvenSRL ps)))


bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

-- instead of Parity and fromParity function, we can use the following
-- record syntax
data AltParity a = AltEven {fromAltParity :: a}
                 | AltOdd  {fromAltParity :: a}
                 | AltNone {fromAltParity :: a}
                   deriving (Show)

-- The Show instance for the variant that uses record syntax is considerably 
-- more verbose!!
{-
    ghci> show $ Even 1
    "Even 1"
    ghci> show $ AltEven 1
    "AltEven {fromAltParity = 1}"
    ghci> length . show $ Even 1
    6
    ghci> length . show $ AltEven 1
    27
-}

-- Chunking a list
-- Each digit in a barcode is encoded with a run of four digits.
-- Flat list that represent a row to four-element lists.
chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs
                 in h : chunkWith f t
                 
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

-- It's somewhat rare that we need to write generic list manipulation functions 
-- like this. Most provided in Data.List itself

-- Generating list of candidates
-- Barcode must start with a black bar (Zero) and must have enough bars  
-- application of bestLeft, bestRight results in empty list => no match then.
-- If there is a match, return a list of lists of parity encoded digits.
-- [elem1 to elem12] 
-- Each of elem1 to elem12 is a list, containing digits ordered by match quality
candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One):_) = []
candidateDigits rle | length rle < 59 = []
candidateDigits rle 
    | any null match = []
    | otherwise      = map (map (fmap snd)) match
  where match = map bestLeft left ++ map bestRight right
        left = chunksOf 4 . take 24 . drop 3 $ runLengths
        right = chunksOf 4 . take 24 . drop 32 $ runLengths
        runLengths = map fst rle
        
        
-- Data.Map (internally using balanced trees)
-- Map k v (key and value)
-- Strict evaluation for keys; lazy evaluation for values.
-- Refer to the piece on "Life without hash and arrays" 

emptyMap = M.empty                          -- fromList []
oneElement = M.singleton "key" "Value"      -- fromList [("key","Value")]
-- fromList 
-- M.fromList :: Ord k => [(k, a)] -> M.Map k a
-- Map is shown using fromList 

-- no pattern matching on map values.
-- 'lookup' function 
-- M.lookup :: (Ord k, Monad m) => k -> M.Map k a -> m a
-- using lookup function on oneElement
findInOneElement inKey = case M.lookup inKey oneElement of 
    Just v -> "Yes, Found"
    Nothing -> "Not Found"

keyFound1 = findInOneElement "key"              -- "Yes, Found"
keyFound2 = findInOneElement "key2"             -- "Not Found"

-- using findWithDefault instead of lookup 
keyFound3 = M.findWithDefault "DefaultMsg: No Such Key" "key" oneElement
keyFound4 = M.findWithDefault "DefaultMsg: No Such Key" "key2" oneElement

-- insert and insertWith' functions
-- insert: gets a key and a value and overwrites the existing value
-- insertWith': gets a combining fn, a key, a value, overwrites with combined 
-- value using the fn, new value and existing value.

map1 = M.insert "foo" "fooVal" oneElement
-- fromList [("foo","fooVal"),("key","Value")]

map2 = M.insert "key" "newVal" oneElement
-- fromList [("key","newVal")]

map3 = M.insertWith' (++) "key" "Val2" map2
-- fromList [("key","Val2newVal")]
-- M.insertWith' :: (Ord k) => (a -> a -> a) -> k -> a -> M.Map k a -> M.Map k a

-- tick (') in M.insertWith' => combining fn is evaluated strictly.
-- M.insertWith -> the lazy variant not usually used - means the value is 
-- stored in a lazy fashion

-- Comment section:
-- insertWith' is now deprecated, because Data.Map has been split into 
-- Data.Map.Lazy and Data.Map.Strict??

-- M.delete :: (Ord k) => k -> M.Map k a -> M.Map k a
-- Deleting the key from the map (along with the value)
map4 = M.delete "key" map1
-- fromList [("foo","fooVal")]

-- Combining two maps using "union" function 
-- If there are same keys in the two maps - the one provided in the first map 
-- argument is prevails. "left biased"
map5 = oneElement `M.union` M.singleton "key2" "val2"
-- fromList [("key","Value"),("key2","val2")]

map6 = oneElement `M.union` map2
-- fromList [("key","Value")]

-- Back to decoding bar codes.
-- Many candidates for the last 12 digits of barcode
-- Also, to use the parity of the first 6 digits to decipher the first digit.

{-
    ghci> product . map length . candidateDigits $ input
    34012224
-}
-- That many brute force combinations!!

-- Solving for check-digit.
type Map a = M.Map Digit [a]
-- key is the check digit, value is the sequence that evaluates to this chk dgt

type DigitMap = Map Digit
type ParityMap = Map (Parity Digit)
-- these are the solution maps.

-- given a single digit, updating the existing solution map.
updateMap :: Parity Digit               -- new digit
          -> Digit                      -- existing key
          -> [Parity Digit]             -- existing digit sequence
          -> ParityMap                  -- map to update
          -> ParityMap                  -- resultant updated map.
          
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a 
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10
    
-- For each digit in a sequence, we'll generate a new solution map, using that 
-- digit and an older solution map.

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit = 
    new `M.union` M.foldWithKey (updateMap digit) M.empty old
    
single :: Digit -> ParityMap     
single n = M.singleton n [Even n]

use1 = useDigit (single 1) M.empty (Even 1)   -- fromList [(2,[Even 1,Even 1])]
use2 = useDigit (single 1) (single 2) (Even 2)
-- fromList [(2,[Even 2]),(3,[Even 2,Even 1])]
-- first (2, [Even 2]) is the dictionary (Map) for (single 2)
-- This new Map is combined with the old map (single 1) - with old map being 
-- adjusted for the 'digit' supplied which is (Even 2) here.

-- a fold using the useDigit 
-- to incorporate or take in all the digits supplied 
incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old digits = foldl' (useDigit old) M.empty digits

inc1 = incorporateDigits (M.singleton 0 []) [Even 1, Even 5]
-- fromList [(1,[Even 1]),(5,[Even 5])]

-- building a complete solution map 
-- refer to the checkDigit definition at the beginning of this chapter.
finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 []) 
            . mapEveryOther (map (fmap (*3))) 
-- the finalDigits should be 11 digit long (leaving the first digit)

firstDigit :: [Parity a] -> Digit
firstDigit = snd 
           . head
           . bestScores paritySRL
           . runLengths
           . map parityBit
           . take 6
   where parityBit (Even _) = Zero
         parityBit (Odd _) = One

-- combine all the digits
addFirstDigit :: ParityMap -> DigitMap
addFirstDigit = M.foldWithKey updateFirst M.empty

updateFirst :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
updateFirst key seq = insertMap key digit (digit:renormalize qes)
    where renormalize = mapEveryOther (`div` 3) . map fromParity
          digit = firstDigit qes
          qes = reverse seq
        
buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -) 
         . addFirstDigit
         . finalDigits

-- finding the correct sequence
solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where checkDigits = map fromParity (last xs)
          m = buildMap (init xs)
          addCheckDigit m k = (++ [k]) <$> M.lookup k m
          
{-
    ghci> listToMaybe . solve . candidateDigits $ input
    Just [9,7,8,0,1,3,2,1,1,4,6,7,7]
-}

-- Working with row data
-- The input to the bar code is a single row from the bar code image.
withRow :: Int -> Pixmap -> (RunLength Bit -> a) -> a
withRow n greymap f = f . runLength . elems $ posterized
    where posterized = threshold 0.4 . fmap luminance . row n $ greymap

row :: (Ix a, Ix b) => b -> Array (a, b) c -> Array a c
row j a = ixmap (l, u) project a
    where project i = (i, j)
          ((l, _), (u, _)) = bounds a
          
-- fmap transforms the values in the array
-- ixmap transforms the indices in the array
-- ixmap is a powerful function for slicing and dicing an array.
-- first argument: bounds of the new array to be formed.
--     here we are converting a 2-Dimensional Array to a 1 dimensional array
-- second argument: projection function
--     which element of old array to be projected as which element of new one.

-- candidateDigits function returns empty unless we call from beginning
-- that can be overcome by this function. Scan till you find a match
findMatch ::  [(Run, Bit)] -> Maybe [[Digit]]
findMatch = listToMaybe
          . filter (not . null)
          . map (solve . candidateDigits)
          . tails
          
{-
    Using tails function and lazy evaluation to our advantage!
    
    *Main> tails [1,2,3,4,5]
    [[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]]
-}  

-- choose a row from the image and try to find the bar code.
findEAN13 :: Pixmap -> Maybe [Digit]
findEAN13 pixmap = withRow center pixmap (fmap head . findMatch)
    where (_, (maxX, _)) = bounds pixmap
          center = (maxX + 1) `div` 2

-- wrapper to print barcode (can be defined as main())
printBarcode :: IO ()
printBarcode = do
    args <- getArgs
    forM_ args $ \arg -> do
        e <- parse parseRawPPM <$> L.readFile arg
        case e of
            Left err -> print $ "Error: " ++ err
            Right pixmap -> print $ findEAN13 pixmap
            
