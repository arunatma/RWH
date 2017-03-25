{-# LANGUAGE MultiParamTypeClasses,GeneralizedNewtypeDeriving, 
             FlexibleInstances #-}

-- Real World Haskell
-- Part of Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html

import SupplyClass
import RandomSupply

-- The Reader Monad
-- Need: A function accepting an environment (e) and returning a value (a)
newtype Reader e a = R {runReader :: e -> a} deriving (Functor, Applicative)
-- needs to have GeneralizedNewtypeDeriving language pragma.

-- if not for GeneralizedNewtypeDeriving, we can write Functor and Applicative
-- instance as below
{-
    instance Functor (Reader e) where
        fmap f mv = do
            v <- mv
            return (f v)
            
    instance Applicative (Reader e) where
        pure = return
        (<*>) mf mv = do
            f <- mf
            v <- mv
            return (f v)
-}

-- Making this a monad instance
instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r
    
-- How to know the piece of code executing in this monad, what's the env?
ask :: Reader e e
ask = R id      -- equivalent of ask x = R id x 
                -- runReader on this will give x
                -- runReader :: Reader e a -> e -> a
                
readerEx1 = runReader (ask >>= \x -> return (x * 3)) 2

                
-- newtype MySupply based on Reader
newtype MySupply e a = MySupply { runMySupply :: Reader e a} deriving
    (Functor, Applicative, Monad)
    
instance MonadSupply e (MySupply e) where
    next = MySupply $ do
             v <- ask
             return (Just v)
    -- or simply:
    -- next = MySupply (Just `liftM` ask)
    
xy :: (Num s, MonadSupply s m) => m s
xy = do
    Just x <- next
    Just y <- next
    return (x * y)
    
{-
    ghci> (fst . runSupply xy) `fmap` randomsIO
    77143018098399107127471567744306069932
-}

-- There are two layers of newtype: MySupply and Reader
runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply

{-
    ghci> runMS xy 4
    16
    ghci> runMS xy 8
    64
    ghci> :t runMS
    runMS :: MySupply i a -> i -> a
    ghci> :t xy
    xy :: (MonadSupply s m, Num s) => m s
-}

