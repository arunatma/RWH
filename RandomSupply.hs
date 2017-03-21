-- Part of Real World Haskell - Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html

-- Using Supply monad as a source of random numbers 

import Supply
import System.Random hiding (next)

-- Get StdGen in IO Monad; Put back a different StdGen when done
-- getStdRandom helps in achieving this
{-
    ghci> :type getStdRandom
    getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
-}

-- 'random' fn gets a new StdGen
-- 'randoms' fn gets an infinite list of random numbers
-- We need: infinite list of random numbers and a new StdGen

-- 'split' fn: Takes a random number generator and gives out two generators

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)
    
-- checking the functionality    
{-
    *Main> (fst . runSupply next) `fmap` randomsIO
    Just 9054501832456227199
  
    *Main> (fst . runSupply next) `fmap` randomsIO
    Just (-7022117854555797024)
    
    *Main> (fst . runSupply next) `fmap` randomsIO
    Just 2741639928019367337
-}
        