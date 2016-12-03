--{-# OPTIONS_GHC -Wall #-}
import Data.List
import Data.Char

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
list17 = and [True, False, True] 				-- False



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
init' [_] = []
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

makeTranspose :: String -> String
makeTranspose x = unlines $ transpose $ lines x

-- Use a fold (choosing the appropriate fold will make your code much simpler) 
-- to rewrite and improve upon the asInt function from the section called 
-- “Explicit recursion”. 1
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
-- groupBy allows user to supply his own equality function
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

