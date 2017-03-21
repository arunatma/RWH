{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Part of Real World Haskell - Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html

-- Hiding the plumbing and implementation
-- Expose only those which are required to be exposed
-- No need to expose get and put functions of the state
module Supply 
    (
      Supply
    , next
    , runSupply
    ) where

import Control.Monad.State    

-- providing a wrapper for State monad
newtype Supply s a = S (State [s] a) deriving (Functor, Applicative, Monad)
-- see that 'S' data constructor is not exposed by this module
-- The consumer will not know that we are internally using the 'State' monad 

-- You cannot just do deriving (Monad).  It needs to be deriving (Functor,
-- Applicative, Monad)

-- execution function    
runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

-- next fn takes the next value from list and gives to the consumer
next :: Supply s (Maybe s)
next = S $  do st <- get
               case st of
                   []      -> return Nothing
                   (x:xs)  -> do put xs
                                 return (Just x)
                                  
-- If we were to write our own unwrap fn and Monad instances
{-
    -- unwrap function for S
    unwrapS :: Supply s a -> State [s] a
    unwrapS (S s) = s

    instance Monad (Supply s) where
        s >>= m = S (unwrapS s >>= unwrapS . m) 
        return  = S . return
        
-}

-- Since Supply is defined as 'newtype' we can automatically derive the 
-- instance of Monad (as updated above)
-- need to have this language pragma {-# LANGUAGE GeneralizedNewtypeDeriving #-}    

-- Some sample runs:
{-
    ghci> runSupply next [1,2,3]
    (Just 1,[2,3])
    ghci> runSupply next [2,3]
    (Just 2,[3])
    ghci> runSupply (liftM2 (,) next next) [1,2,3]
    ((Just 1,Just 2),[3])
    ghci> runSupply (liftM3 (,,) next next next) [1,2,3]
    ((Just 1,Just 2,Just 3),[])
    ghci> runSupply (liftM3 (,,) next next next) [1,2]
    ((Just 1,Just 2,Nothing),[])
-}

