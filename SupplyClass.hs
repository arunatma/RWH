{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, 
             FunctionalDependencies #-}

-- Real World Haskell
-- Part of Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html

-- Separating interface from implementation
module SupplyClass
    (
      MonadSupply(..)
    , S.Supply              -- defined in another; exported in this module!
    , S.runSupply           -- defined in another; exported in this module!
    ) where
    
import qualified Supply as S 

class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)
    
-- Making Supply an instance of MonadSupply    
-- Need to use FlexibleInstances language pragma because the one which is to 
-- made and instance of (MonadSupply s) has a parameter
instance MonadSupply s (S.Supply s) where
    next = S.next
    
-- Type checker now checks for functional dependency
-- 'm' should be used with 's'. Here m is eq of (S.Supply s) 

-- So, (S.Supply Int) cannot be an instance of MonadSupply
-- Compiler checks that (S.Supply x) should be an instance of (MonadSupply x)

showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)

{-  
    ghci> S.runSupply showTwo_class [1,2]
    ("\"a: \"Just 1, b: Just 2",[])
-}
-- see the use of S.runSupply and not runSupply above!
