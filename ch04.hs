{-# OPTIONS_GHC -Wall #-}
import Data.List
import Data.Char
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
-- TODO : The following function does not work as expected, need to correct.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' f (x:xs) = foldr (groupFn f x) [] xs where
    groupFn fn y1 y2 ys
        | fn y1 y2 = y1:ys
        | otherwise = []
        

-- How many of the following Prelude functions can you rewrite using list 
-- folds? 
-- any 
-- cycle
-- words 
-- unlines
-- For those functions where you can use either foldl' or foldr, which is more 
-- appropriate in each case?        

