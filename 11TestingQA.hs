-- Real World Haskell
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html

--------------------------------------------------------------------------------
-- Imports
import Test.QuickCheck
import Data.List
--------------------------------------------------------------------------------

-- Haskell tools to build precise systems:
-- Expressive type system (allows constraints to be enforced statically)
-- Pure Functional paradigm
-- Polymorphism (both this and pure fn. paradigm) makes code modular, refactorable
-- Unit Testing (HUnit library)
-- Property based testing (QuickCheck library) - abstract test constraints are
--    defined here and the library generates random test cases

-- QuickCheck
-- Type Based Testing

-- quick sort function to test
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = 
	let lhs = filter (<x) xs
--		rhs = filter (>=x) xs
    in qsort lhs ++ [x] ++ qsort lhs

-- Properties to test
-- 1. Idempotency: applying the function twice has the same result as applying it
--                 once
-- Quick sort is a stable algorithm - so, this property should hold good

-- Written as,
prop_idempotent :: (Ord a, Eq a) => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

emptyTest :: Ord a => [a] -> Bool
emptyTest = prop_idempotent []						-- True
allSameTest = prop_idempotent [1, 1, 1, 1] 			-- True
intTest = prop_idempotent [1..100]					-- True
randomTest = prop_idempotent [1,2,4,5,3,4,1]		-- True
-- Tedious and cumbersome to write test cases and maintain 


