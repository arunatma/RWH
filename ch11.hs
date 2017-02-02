-- Real World Haskell
-- Chapter 11: Testing and Quality Assurance

-- Tools at our disposal (for QA and precise code)
-- Expressive type system of Haskell
-- Functional Purity
-- Polymorphism
-- Inherent encouragement of abstraction in the language
-- Modular, Refactorable and Testable code

-- Unit testing via HUnit library
-- Property based testing via QuickCheck library

import Test.QuickCheck
import Data.List

-- testing a custom sort function
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

-- this is a nice example code to show functional programming elegance

-- We need to check whether this follows some basic sorting properties
-- idempotency: applying a function twice should have the same result as 
--              applying it once
prop_idempotent xs = qsort (qsort xs) == qsort xs

{-
    prop_idempotent [] 
    prop_idempotent [1,1,1,1]
    prop_idempotent [1..100]
    prop_idempotent [6,3,76,4,8,2,1] 
-}

-- But instead of testing each manually, we need to have it generated 
-- automatically. QuickCheck helps in the same
{-
    ghci> :type quickCheck
    quickCheck :: (Testable prop) => prop -> IO ()
-}
-- quickCheck function just needs a property to be tested

-- Testing 100 lists of Integer
idemTest1 = quickCheck (prop_idempotent :: [Integer] -> Bool)
-- Testing 100 lists of Char
idemTest2 = quickCheck (prop_idempotent :: [Char] -> Bool)

-- verboseCheck (same functionality as quickCheck)
-- (here in verboseCheck we can see what those inputs which are tested)
idemTest3 = verboseCheck (prop_idempotent :: [Integer] -> Bool)
idemTest4 = verboseCheck (prop_idempotent :: [Char] -> Bool)

-- Other Properties
-- 1. First element of sorted list should always be the min value of the list
prop_minimum xs = head (qsort xs) == minimum xs
minTest1 = quickCheck (prop_minimum :: [Integer] -> Bool)
-- minTest1 may fail as 'head' function is not safe for empty list

-- we shall test for only the non empty lists
-- The test to be filtered can be done using ==> function
-- (==>) :: Testable prop => Bool -> prop -> Property
prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs
minTest2 = quickCheck (prop_minimum' :: [Integer] -> Property)

-- note that the types of prop_minimum and prop_minimum' are different
-- one gives a "Bool" and the other gives a "Property"
-- "Property" is a function that takes in Bool and prop and 
-- filters out the non empty lists here

-- 2. The output list should be ordered (each successive element to be greater
--    than or equal to the current element)
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x < y && ordered (y:xs)
          
-- 3. Output sorted list is a permutation of the input
--    (done using list difference operator (\\) and checking for null)
prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)
          
-- 4. The last sorted element should be the largest element
prop_maximum' xs = not (null xs) ==> last (qsort xs) == maximum xs

-- 5. Given two lists, smallest element of the two should be the first one if 
--    the two lists are appended and sorted
prop_append xs ys = 
    not (null xs) ==> 
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
testAppend = quickCheck (prop_append :: [Integer] -> [Integer] -> Property)

-- Testing against a model
-- Testing against a reference function having the same properties
-- We shall test qsort against the haskell inbuilt sort functionality
prop_sort_model xs = sort xs == qsort xs
testModel = quickCheck (prop_sort_model :: [Integer] -> Bool)

-- Model-based testing is extremely powerful. Often developers have a reference 
-- implementation or prototype that, while inefficient, is correct.  We write
-- a better implementation and want to ascertain the functionality of model

-- Testing Case Study
-- Specifying a pretty printer
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

-- making use of "Arbitrary" type class from QuickCheck library
-- This type class provides "arbitrary" function
{-
    class Arbitrary a where
      arbitrary   :: Gen a         
-}
-- This helps in generating data of any type 'a'

-- The random generators run in "Gen" monad context
-- We would require the following functions, defined in the QuickCheck library
{-
    elements :: [a] -> Gen a
    choose   :: Random a => (a, a) -> Gen a
    oneof    :: [Gen a] -> Gen a
-}    

-- a sample data type
data Ternary 
    = Yes
    | No
    | Unknown
    deriving (Eq, Show)
    
-- providing an "Arbitrary" instance for this data type
instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]

-- another way of providing "Arbitrary" instance via "choose" function    
{- 
    instance Arbitrary Ternary where
        arbitrary = do
            n <- choose (0, 2) :: Gen Int
            return $ case n of
                          0 -> Yes
                          1 -> No
                          _ -> Unknown
-}

-- That was to generate a single random value for Ternary
-- To generate a random pair of random values (a, b)
{- 
    instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
        arbitrary = do
            x <- arbitrary
            y <- arbitrary
            return (x, y)
-}

-- instance for Char (already defined in the library)
{-
    instance Arbitrary Char where
        arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")
    
-}

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

-- A better implementation of the same, using "oneof"
{-

    instance Arbitrary Doc where
        arbitrary =
            oneof [ return Empty
                  , liftM  Char   arbitrary
                  , liftM  Text   arbitrary
                  , return Line
                  , liftM2 Concat arbitrary arbitrary
                  , liftM2 Union  arbitrary arbitrary ]

-}

