--{-# OPTIONS_GHC -Wall #-}

-- Chapter 4. Fuctional Programming
-- http://book.realworldhaskell.org/read/functional-programming.html


import Data.List
import Data.Char
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

-- Have a look at ch04InteractWith.hs
-- The program takes command line arguments: Input File and Output File 
-- Takes the input file and process with a defined function and writes into 
-- the output file. The processing function is pure code and the other parts 
-- deal with the real world.

-- Splitting lines 

splitEx1 :: [String]
splitEx1 = lines "line 1\nline 2"      -- ["line1", "line2"]
splitEx2 :: [String]
splitEx2 = lines "foo\n\nbar\n"        -- ["foo", "", "bar"]
-- lines work in text mode only
splitEx3 :: [String]
splitEx3 = lines "a\r\nb"              -- ["a\r", "b"]
-- In Windows: 
-- Reading "\r\n" (carriage return; new line) translates to "\n" in text mode 
-- Writing "\n" to a file happens as "\r\n"
-- In Unix no such translation or reverse translation happens
-- Both readFile and writeFile operations work in text mode 

-- So,
-- If we read a Windows-generated text file on a Linux or Unix box, we'll get 
-- trailing carriage returns at the end of each line

-- Implementing a function, to take care of the difference
splitLines :: String -> [String]
splitLines [] = []
splitLines cs = 
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'                

-- break function
-- Takes in a predicate and list inputs. Tests for each element of list 
-- It breaks into a tuple of two lists on the first successful match. With the 
-- first successful matching element as the first element in the second list 
breakEx1 = break odd [2,4,5,6,8]        -- ([2,4], [5,6,8])
breakEx2 = break isUpper "isUpper"      -- ("is", "Upper")

-- fixLines - remove \r\n or \n from the string; 
fixLines :: String -> String
fixLines input = unlines (splitLines input)

-- gpl-3.0.txt written in Unix system (not readable in windows notepad)
-- fix it using the fixLines function (used along with ch04InteractWith.hs)
-- Method 1: Compile into an "exe" and run
    -- > ghc --make ch04InteractWith.hs
    -- > ./ch04InteractWith.exe

-- Method 2: Run from commandline without creating "exe"
    -- > runhaskell ch04InteractWith.hs

-- Now the output is readable in windows.

-- On Unix-like systems, the standard pagers and editors hide Windows line 
-- endings. So our version of fixLines may not help
{-
    $ file gpl-3.0.txt
    gpl-3.0.txt: ASCII English text
    
    $ unix2dos gpl-3.0.txt
    unix2dos: converting file gpl-3.0.txt to DOS format ...
    
    $ file gpl-3.0.txt
    gpl-3.0.txt: ASCII English text, with CRLF line terminators    
-}


-- Usual function notation: prefix - writing name of the function followed by
-- the arguments
-- infix - writing the function name between the arguments Ex: +, * etc

-- data Pair a b = Pair a b deriving (Show)     -- Pair 1 2
data a `Pair` b = a `Pair` b deriving (Show)    -- 1 `Pair` 2

pair1 = Pair 1 2
pair2 = True `Pair` 2          -- using the value constructor fn as infix

-- using the elem function (to find that an element is part of the given list)
elem1 = elem 'a' "arunram"
elem2 = elem 1 [1, 2, 3]

-- using as infix 
elem3 = 3 `elem` [1, 2, 4, 8]

-- Special functions from Data.List (to check for prefix or infix)
infix1 = "hay" `isInfixOf` "Make hay while the sun shines"
prefix1 = "Learn" `isPrefixOf` "Learn you a haskell"
suffix1 = [2,3] `isSuffixOf` [1,2,3]

-- List Functions

-- ghci> :module +Data.List
-- ghci> :m +Data.List

list1 = length [1,2,3,4,5]                      -- 5
list2 = null []                                 -- True
list3 = head [1,2,3]                            -- 1        [a] -> a
list4 = tail [1,2,3]                            -- [2,3]    [a] -> [a]
list5 = last [1,2,3]                            -- 3        [a] -> a
list6 = init [1,2,3]                            -- [1,2]    [a] -> [a]
    
list7 = head []                                 -- throws Error 

dumbHead xs = if length xs > 0 then head xs else 'z'  
list8 = dumbHead []                             -- 'z'
-- but this dumbHead is very costly. To find length, entire list to be parsed

goodHead xs = if null xs then 'z' else head xs
list9 = goodHead []                             -- 'z'

-- using pattern matching 
goodHead2 (x:_) = x
goodHead2 []    = 'z'

list10 = goodHead2 []                           -- 'z'

-- Partial functions
-- Functions that have return values defined only for a subset of inputs 
-- 'head' is a partial function. It does not have value defined for []

-- Total Functions
-- Functions where all values in input domain have return values defined

-- A tip: use "unsafe" prefix when defining partial functions, so you know that 
-- you may get a run time error when a non defined input is applied to the fn.

-- append (++)
list11 = [1,2] ++ [3,4]                         -- [1,2,3,4]
list12 = [True] ++ []                           -- [True]

-- concat
list13 = concat [[1,2], [3], [4,5,6]]           -- [1,2,3,4,5,6]
list14 = concat ["a", "beautiful", "river"]     -- "abeautifulriver"

list15 = reverse [1,2,3]                        -- [3,2,1]
list16 = reverse "hello"                        -- "olleh"


-- and :: [Bool] -> Bool
list17 = and [True, False, True]                -- False

-- or :: [Bool] -> Bool
list18 = or [True, False, False]                -- True
list19 = or []                                  -- False

-- all :: (a -> Bool) -> [a] -> Bool
-- Test whether all elements in a list satisfying a criteria
list20 = all odd [1, 3, 5, 7]                   -- True
list21 = all odd [1, 3, 6, 9]                   -- False

-- any :: (a -> Bool) -> [a] -> Bool
-- Test whether any of the elements in a list satisfying a criteria
list22 = any even [1, 3, 5, 7]                  -- False
list23 = any even [1, 3, 6, 9]                  -- True

-- Working with sub lists
-- take and drop functions
list24 = take 3 "india"                         -- "ind"
list25 = drop 2 "india"                         -- "dia"
list26 = take 5 [1,2..]                         -- [1,2,3,4,5]
list27 = take 10 [100,99..]                     -- [100,98..91]
list28 = drop 1 [True, False, False]            -- [False, False]

-- splitAt : combination of both take and drop functions
-- Returns a tuple with both lists
splitAtEx1 = splitAt 2 "india"                  -- ("in", "dia")

-- A possible definition of splitAt function 
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

-- takeWhile, dropWhile functions
-- Both take a predicate and list as input 
-- Applies predicate to each element checks for True
-- takeWhile takes as long as the predicate returns True 
-- dropWhile drops as long as the predicate returns True
list29 = takeWhile odd [1,3,5,6,8,9,11]         -- [1, 3, 5]
list30 = dropWhile odd [1,3,5,6,8,9,11]         -- [6, 8, 9, 11]

-- span : combination of both takeWhile and dropWhile functions
-- Returns a tuple with both lists
spanEx1 = span odd [1,3,5,6,8,9,11]             -- ([1,3,5], [6,8,9,11])

-- A possible definition of span function
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' pred xs = (takeWhile pred xs, dropWhile pred xs)


-- Searching the lists
-- elem : returns True when the element is present in the list 
-- notElem : returns True when the element is not present in the list 
elem4 = 4 `elem` [1,2,3,4]                      -- True
elem5 = 5 `notElem` [1,2,3,4]                   -- True
elem6 = not (elem 5 [1,2,3,4])                  -- True 

-- filter: retruns all the elements which match the given predicate
filterEx1 = filter odd [1,2,3,4,5,6]                    -- [1,3,5]
filterEx2 = filter (\x -> notElem x "aeiou") "Haskell"  -- "Hskll"

-- Already seen:
-- isPrefixOf
-- isSuffixOf
-- isInfixOf

-- Working with several lists
-- zip: takes two lists and zips them together in the form of pairs
-- output: list of pairs 
zipEx1 = zip "arun" "ram"                   -- [('a','r'), ('r','a'), ('u','m')]
zipEx2 = zip "Me" [1,2,3,4,5]               -- [('M', 1), ('e', 2)]

-- zipWith
-- given an operator, so it merges the elements in the tuple with that operator
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipEx3 = zipWith (+) [1,2,3] [4,5,6]        -- [5,7,9]
zipEx4 = zipWith (,) "Me" [1,2,3,4,5]       -- [('M', 1), ('e', 2)]

-- So effectively "zip" written using "zipWith" function 
zip' = zipWith (,)

-- "zipWith" written using "zip"
zipWith' f xs ys = map (\(x,y) -> f x y) $ zip xs ys

-- Haskell Defines to zip multiple lists 
-- zip3 to zip7 available to merge 3 lists  to 7 lists.
-- zipWith3 to zipWith7 available to operate on 3 lists to 7 lists.

-- Special string functions
-- lines and unlines 
line3 = lines "foo\nbar"                    -- ["foo", "bar"]
line4 = unlines ["foo", "bar"]              -- "foo\nbar\n"
-- unlines always add \n to each string inside the list

wordEx1 = words "the  \r  quick \t  brown\n\n\nfox"
wordEx2 = unwords wordEx1                   -- "the quick brown fox"
-- unwords just put a single white space between strings and note that there is
-- no white space at the end. That way, the operation of unlines and unwords 
-- differ in how they treat the last element.

--------------------------------------------------------------------------------
-- Exercises from
-- http://book.realworldhaskell.org/read/functional-programming.html
-- Repeating the questions part in this file, for easy read.

-- Write your own “safe” definitions of the standard partial list functions, 
-- but make sure that yours never fail. As a hint, you might want to consider 
-- using the following types.
-- safeHead :: [a] -> Maybe a
-- safeTail :: [a] -> Maybe [a]
-- safeLast :: [a] -> Maybe a
-- safeInit :: [a] -> Maybe [a]

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init' xs)

init' :: [a] -> [a]
init' [] = []           -- This line will never be used when called by safeInit
init' (_:[]) = []
init' (x:xs) = x : init' xs

sTail :: [a] -> [a]
sTail (_:xs) = xs
sTail _ = []


-- Write a function splitWith that acts similarly to words, but takes a 
-- predicate and a list of any type, and splits its input list on every element 
-- for which the predicate returns False.
splitWith :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitWith f xs = filter (/=[]) (separateItems f xs)

separateItems :: (a -> Bool) -> [a] -> [[a]]
separateItems _ [] = []
separateItems f xs =  (fst broken) : separateItems f (sTail (snd broken))
                        where broken = break f xs

-- Using the command framework from the section called “A simple command line 
-- framework”, write a program that prints the first word of each line of its 
-- input. 

-- To do this one.

-- Write a program that transposes the text in a file. For instance, it should 
-- convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n". 
makeTranspose :: String -> String
makeTranspose x = unlines $ transpose $ lines x
--------------------------------------------------------------------------------

-- How to think about loops?

-- No loops at all in Haskell.
-- Looping kind of operations to be done using other means. (explicit recursion,
-- folds, transform each element to something else, filter a set of inputs 
-- based on a condition)

-- 1. Explicit Recursion

-- C function that takes string input ("23") and converts to integer (23).
{-

int as_int(char *str)
{
    int acc; /* accumulate the partial result */

    for (acc = 0; isdigit(*str); str++) {
        acc = acc * 10 + (*str - '0');
    }

    return acc;
}

-}

-- Let us create a function "loop" in Haskell which parse through the chars
-- and sums up the values.
asInt :: String -> Int
asInt xs = loop 0 xs 

-- first parameter to loop is the accumulator: initialized to 0 in this case 
-- just as acc = 0 in the C equivalent
loop :: Int -> String -> Int 
loop acc [] = acc
loop acc (x:xs) = loop acc' xs 
    where acc' = acc * 10 + digitToInt x
    
-- Last thing that the "loop" does is calling itself. It is an example of 
-- "tail call recursion"

-- Thinking on the structure of the list and handling the empty and non-empty 
-- cases separately - This approach is called "structural recursion"

-- This "loop" function is an example of both tail call recursion and structural
-- recursion.

-- "Base case" - means non-recursive case - here: loop acc [] = acc 
-- "Recursive case" or "Inductive case" - where the function calls itself

{- 
TAIL CALL RECURSION:

In traditional languages, the loops execute in constant space, whereas in 
a recursive function, it needs to have the thunks stored, so the memory 
requirements increase with the size of the loop. 

Functional language implementation detects the uses of tail recursion, and 
transforms all recursive calls to run in constant space. this is called 
tail call optimization - "tco"

Very few imperative language has support for TCO, so a similar recursive style
applied in tranditional languages lead to memory leaks and poor performance

-}

-- 2. Transforming Every Piece of Input 

-- Considering another C function, to square numbers 
{- In place squaring of numbers, given an address location

void square(double *out, const double *in, size_t length)
{
    for (size_t i = 0; i < length; i++) {
	out[i] = in[i] * in[i];
    }
}

-}

-- every input element is substituted with a different element 
square :: [Double] -> [Double]
square (x:xs) = x * x : square xs 
square []     = []

-- converting each character to uppercase
{-
-- #include <ctype.h>

char *uppercase(const char *in)
{
    char *out = strdup(in);
    
    if (out != NULL) {
	for (size_t i = 0; out[i] != '\0'; i++) {
	    out[i] = toupper(out[i]);
	}
    }

    return out;
}
-}

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []

-- "base case" deals with the empty list 
-- "recursive case" deals with the non-empty list 

-- 3. Mapping over a list 

-- For both "square" and "upperCase" functions, lengths of the input and output 
-- list are the same.  Each element is transformed into something else 
-- This can be done by mapping a function over the list.
square2 xs = map (\x -> x * x) xs
uppercase2 xs = map toUpper xs

-- map :: (a -> b) -> [a] -> [b]
-- map function takes another function as input (just like zipWith)
-- map, zipWith are higher order functions, taking another fn as input 
-- Higher order functions : Take another function as input; or return another 
-- function as output.

-- map could have been defined like this 
map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs
map' _ _      = []              -- If it does not form a pattern of (x:xs)
-- f is not used on right side, so "_"
-- default if (x:xs) not matched, it is empty list, no need to match, so "_"


-- As a matter of style, it is fine to use wild cards for well known simple 
-- types like lists and Maybe. For more complicated or less familiar types, it 
-- can be safer and more readable to name constructors explicitly

-- 4. Selecting Pieces of Input 

-- select odd numbers from a list
oddList :: [Int] -> [Int]
oddList (x:xs) 
    | odd x         = x : oddList xs
    | otherwise     = oddList xs
oddList _           = []       
 
-- select non-vowels from a string
nonVowels :: String -> String 
nonVowels (x:xs)
    | notElem x "aeiou" = x : nonVowels xs
    | otherwise         = nonVowels xs
nonVowels _             = []

-- The idea is same, take each element, check for a condition, select or not.
-- "filter" function abstracts this idea.
-- filter :: (a -> Bool) -> [a] -> [a]

-- oddList Rewritten
oddList2 = filter odd 

list31 = oddList [1,1,2,3,5,8,13,21,34]
list32 = oddList2 [1,1,2,3,5,8,13,21,34]

-- 5. Computing one answer over a collection
-- Reducing the collection to a single value (using "fold")
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc

sumVal1 = mySum [1,2,3,4,5]                     -- 15

-- Adler-32 checksum implementation
-- Concatenation of 2 16-bits
-- First 16-bit: Sum of input bytes + 1 
-- Second 16-bit: Sum of intermediate values of first check sum
-- Sums are computed to the modulo of 65521
 
{-
public class Adler32 
{
    private static final int base = 65521;

    public static int compute(byte[] data, int offset, int length)
    {
        int a = 1, b = 0;

        for (int i = offset; i < offset + length; i++) {
            a = (a + (data[i] & 0xff)) % base;
            b = (a + b) % base;
        }

        return (b << 16) | a;
    }
}
-}


-- See above, there are a few functions to be imported
-- imports to be at top, so commented here
-- import Data.Char (ord)
-- import Data.Bits (shiftL, (.&.), (.|.))
          
base = 65521          
adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = helper a' b' xs 
            where a' = (a + (ord x .&. 0xff)) `mod` base
                  b' = (a' + b) `mod` base
          helper a b _   = (b `shiftL` 16) .|. a
        
-- Using a pair, instead of 2 accumulator variables
adler32' xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
              let a' = (a + (ord x .&. 0xff)) `mod` base
                  b' = (a' + b) `mod` base
              in helper (a',b') xs
          helper (a,b) _     = (b `shiftL` 16) .|. a        

-- Change in the 2nd function: Using a single accumulator, instead of 2.
-- "Do something to every element of a list, update acc as we go, returning 
-- the accumulator as we are done"
-- This can be abstracted to a function called "fold"

-- Reduce all elements of type 'b' in a list to a single value of type 'a'
-- The accumulator and the reduced (folded up) value are of same type.
-- also take a function as argument which takes in a 'a' and a 'b' and gives 
-- out a 'a'.  The function takes an accumulator and an element from the list.

-- Here is a custom definition of "foldl", the left fold
myFold :: (a -> b -> a) -> a -> [b] -> a
myFold step zero (x:xs) = myFold step (step zero x) xs
myFold _ zero []        = zero

-- reimplementation of "sum" function using the foldl function
foldSum xs = foldl step 0 xs
    where step acc x = acc + x
    
-- step function replaced by (+)
niceSum xs = foldl (+) 0 xs

sumVal2 = niceSum [1,2,3,4,5]                     -- 15          
-- compare the niceSum and foldSum with mySum, niceSum is so concise.
-- the explicit recursion part is taken care of 'fold'
-- Any repeated pattern can be abstracted to a function!
-- just need to worry about the accumulator and the step function.

{-
foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)
-}

{-
-- rewriting the adler32' function using foldl
-- check this, throwing up errors.
adler32Fold xs = let (a, b) = foldl step (1, 0) xs
                 in (b `shiftL` 16) .|. a
                    where step (a, b) x = let a' = a + (ord x .&. 0xff)
                                          in (a' mod base, (a' + b) `mod` base)
                          
-}

-- right fold (foldr                  
myFoldr :: (a -> b -> b) -> b -> [a] -> b

myFoldr step zero (x:xs) = step x (myFoldr step zero xs)
myFoldr _    zero []     = zero
          
{-
foldr (+) 0 (1:2:3:[])
          == 1 +           foldr (+) 0 (2:3:[])
          == 1 + (2 +      foldr (+) 0 (3:[])
          == 1 + (2 + (3 + foldr (+) 0 []))
          == 1 + (2 + (3 + 0))

Simply put,
                 1 : (2 : (3 : []))
                 1 + (2 + (3 + 0))

foldr replaces the empty list with the zero value, and every constructor in the 
list with an application of the step function

-}

-- Rewriting filter with explicit recursion 
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p []   = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

-- now, instead of recursion, we can use fold.
foldFilter :: (a -> Bool) -> [a] -> [a]
foldFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys

-- using foldl
foldFilter' :: (a -> Bool) -> [a] -> [a]
foldFilter' p xs = reverse $ foldl step [] xs
    where step ys x | p x       = x : ys
                    | otherwise = ys
           
-- Note that the arguments for step functions are interchanged in foldr v foldl

-- 'map' function written using foldr
mapUsingFold :: (a -> b) -> [a] -> [b]
mapUsingFold f xs = foldr step [] xs
    where step x ys = f x : ys

-- 'foldl' function written using foldr
foldlWithr :: (a -> b -> a) -> a -> [b] -> a
foldlWithr f z xs = foldr step id xs z
    where step x g a = g (f a x)
{-        

-- Yet to decode how foldl construction happens with foldr 

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-}

-- getting the same list back, using foldr 
identity :: [a] -> [a]
identity xs = foldr (:) [] xs

-- equivalent of (++)
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs  -- ys is the accumulator, xs will be expanded


-- Disadvantages of foldl
{-
foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)
-}
-- Final expression is not evaluated until the entire list is traversed.
-- Must be stored as thunk, thunk is expensive in storage
-- foldl' overcomes the issue, by making it stricter evaluation
          
  
-- Use a fold (choosing the appropriate fold will make your code much simpler) 
-- to rewrite and improve upon the asInt function from the section called 
-- "Explicit recursion". 1
asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold (x:xs) 
    | x == '-' = 0 - processInt xs
    | otherwise = processInt (x:xs)
    
processInt :: String -> Int    
processInt xs = foldl (convertInt) 0 (map digitToInt (xs))

convertInt :: Int -> Int -> Int
convertInt x y = y + 10 * x

-- The asInt_fold function uses error, so its callers cannot handle errors. 
-- Rewrite it to fix this problem
type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Right 0
asInt_either (x:xs) 
    | all isDigit (x:xs)            = Right $ asInt_fold (x:xs)
    | x == '-' && all isDigit xs    = Right $ asInt_fold (x:xs)
    | otherwise                     = Left "Non Digit"
    
-- The Prelude function concat concatenates a list of lists into a single list, 
-- and has the following type. Write your own definition of concat using foldr. 
concatOwn :: [[a]] -> [a]
concatOwn xs = foldr (++) [] xs

-- Write your own definition of the standard takeWhile function, first using 
-- explicit recursion, then foldr
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) 
    | f x = x : takeWhile' f xs
    | otherwise = []
    
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr (takeMe f) [] xs where 
    takeMe fn x ys 
        | fn x = x : ys
        | otherwise = []
    
-- The Data.List module defines a function, groupBy, which has the following 
-- type. Write your own implementation using a fold.
-- group "Mississippi" == groupBy (\x y -> x == y) "Mississippi" == 
-- ["M","i","ss","i","ss","i","pp","i"]
-- groupBy allo ws user to supply his own equality function
{-
-- TODO : The following function does not work as expected, need to correct.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' f (x:xs) = foldr (groupFn f x) [] xs where
    groupFn fn y1 y2 ys
        | fn y1 y2 = y1:ys
        | otherwise = []
-}

-- How many of the following Prelude functions can you rewrite using list 
-- folds? 
-- any 
-- cycle
-- words 
-- unlines
-- For those functions where you can use either foldl' or foldr, which is more 
-- appropriate in each case?        

-- Anonymous lambda functions.
isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s
    
-- instead of defining inSequence helper function using a "where" clause
isInAny2 needle haystack = any (\x -> needle `isInfixOf` x) haystack

-- Restrictions on lambda functions.
-- Normal functions can have any number of clauses and pattern, guards.
-- Lambda function can have only a single clause in its definition.

-- To help readability and maintainability, it is better to avoid lambda fns.

-- Partial Function Applications and Currying.
-- :type dropWhile
-- dropWhile :: (a -> Bool) -> [a] -> [a]

-- Looks like the symbol "->" is separating the parameters and the result.
-- "->" actually says this: The thing on the left is input and the things on 
-- the right is the output 

-- "->" always takes one argument.  So, all functions in Haskell takes just 
-- one argument.

-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- Takes a predicate, and a list as input and returns another list as output.
-- (a -> Bool) -> ([a] -> [a])
-- Takes a predicate as input and returns a function that takes a list as input 
-- returns a list as output.
-- While dropWhile looks like a function that takes two arguments, it is 
-- actually a function of one argument, which returns a function that takes 
-- one argument 


-- dropOnSpace strips the leading white spaces
dropOnSpace :: [Char] -> [Char]
dropOnSpace = dropWhile isSpace

leftTrimStr = map dropOnSpace ["   india", "united states", "   japan"]

-- trailTrim strips the trailing white spaces
trailTrim :: [Char] -> [Char]
trailTrim = reverse . dropOnSpace . reverse

rightTrimStr = map trailTrim ["india   ", "united states  ", "japan"]

-- trim both sides
trim = dropOnSpace . trailTrim

trimmedStr = map trim ["  india  ", "  united states  "]

-- zip3 needs 3 arguments 
-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zippedEx1 = zip3 "four" "five" "nine"   -- [(f,f,n), (o,i,i), (u,v,n), (r,e,e)]

-- partial application (applying less arguments than a function can accept)
-- pre-supply one argument to zip3 (zipWithAnt is a function that takes 
-- 2 strings and gives out the zipped list of tuples.
zipWithAnt = zip3 "ant"
zippedEx2 = zipWithAnt "cat" "rat"      -- [(a,c,r), (n,a,a), (t,t,t)]

-- Using partial application instead of lambda 
isInAny3 needle haystack = any (isInfixOf needle) haystack

-- Partial application of Haskell functions is called "currying"

-- foldl is applied partly the list argument is not given.
-- So, "foldl (+) 0" expects a list as input, means nicerSum expects a 
-- list as input.
nicerSum :: [Integer] -> Integer
nicerSum = foldl (+) 0

-- "Section"  - partial application in the infix functional notation.
sectionEx1 = (1+) 2             -- only the first argument of (+) is supplied
sectionEx2 = map (2*) [1,2,3]   -- [2,4,6]
sectionEx3 = map (*3) [1,2,3]   -- only the 2nd argument of (*) is supplied
sectionEx4 = map (2^) [0,1,2,3] -- Powers of 2
sectionEx5 = map (^2) [0,1,2,3] -- Squares

-- We can use any function in the infix notation to get benefit of "sectioning"
sectionEx6 = all (`elem` ['a'..'z']) "Frobozz"  -- False

-- Using sectioning, rewriting isInAny function.
isInAny4 needle haystack = any (needle `isInfixOf`) haystack

-- "As-patterns"
-- xs@(_:xs') is called as-pattern 
-- Bind the variable xs to the value that matches the right side of '@'
-- xs is bound to entire list and xs' is bound to all but the head.

tailEx1 = tails "cricket"   -- ["cricket", "ricket", "icket" .. "et", "t", ""]

-- rewriting the tails function (not outputting the last empty string)
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _          = []

-- using the tails function 
suffixes2 xs = init (tails xs)

-- applying a function, and on the result apply another function.
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f (g x)

-- using compose 
suffixes3 xs = compose init tails xs 

-- xs is common on both sides, no need to give explicitly 
suffixes4 = compose init tails 

-- there is a built-in compose function available (.)
suffixes5 = (.) init tails

-- using the compose (.) function in infix form 
suffixes6 = init . tails

-- Puzzle: Count the number of words in a string that begins with Capital letter
numCapStarts xs = length (filter (==True) (capStart xs))
    where capStart xs = map isUpper (map head (words xs))

-- Better formulation of the same function using (.)
-- (.) is right associative 
numCapStarts2 = length . filter (isUpper . head) . words
    
-- To get the names of the macros in the C file 

-- #define DLT_EN10MB      1       /* Ethernet (10Mb) */
-- #define DLT_EN3MB       2       /* Experimental Ethernet (3Mb) */
-- #define DLT_AX25        3       /* Amateur Radio AX.25 */

dlts :: String -> [String]
dlts = foldr step [] . lines
    where step l ds | "#define DLT_" `isPrefixOf` l = secondWord l : ds
                    | otherwise                     = ds
          secondWord = head . tail . words

inputLines = "#define DLT_EN10MB      1       /* Ethernet (10Mb) */\n#define DLT_EN3MB       2       /* Experimental Ethernet (3Mb) */\n#define DLT_AX25        3       /* Amateur Radio AX.25 */"

macroNames = dlts inputLines

-- Some tips:
-- Use explicit tail recursion and lambda functions sparingly
-- Use map, filter functions wherever possible instead of tail recursion.
-- fold takes more effort to understand than map or filter
-- Use fold, in place of a tail recursion loop.

-- Avoiding space leaks with "seq" function
-- "seq" function forces left side argument to be evaluated before looking @ RHS
-- The foldl' (the strict version of foldl) may be implemented as follows

foldlStrict _    zero []     = zero
foldlStrict step zero (x:xs) =
    let new = step zero x
    in  new `seq` foldlStrict step new xs
    
-- :t seq 
-- seq :: a -> t -> t   (gives the second argument as result)
-- But the first thing that it does is to evaluate 'a'


    