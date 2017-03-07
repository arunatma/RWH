-- Logger.hs
-- Part of chapter 14: Monads
-- http://book.realworldhaskell.org/read/monads.html

module Logger 
    (
      Logger
    , Log
    , runLogger
    , record
    ) where

import Control.Monad
    
-- Though there may be different functions, only the above data types / fns 
-- are exposed. Easy to deal with.  Free to choose internal implementation.

-- Logger is a pure type constructor. User can have his own value constructor.

type Log = [String]

-- Monad typeclass does not provide a function to escape from the monad context
-- bind produces another monadic value
-- inject gets a normal value and produces a monadic value
-- There is no fn that gets a monadic value and gives out a normal value 

-- runLogger is written to extract information from the monadic context
-- This is called "an execution function".  There can be several execution fns
-- for a single monad 


-- This was globToRegex :: String -> Bool -> String in Chapter 8 GlobRegex.hs 
globToRegex :: String -> Logger String
globToRegex cs = globToRegex' cs >>= \ds ->
                    return ('^':ds)
           
{-

    ghci> :type (>>=)
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
    ghci> :type (globToRegex "" >>=)
    (globToRegex "" >>=) :: (String -> Logger b) -> Logger b

    So, the anonymous function in globToRegex (\ds ->) is of type 
    (String -> Logger b) 
-}

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) = record "any" >>         -- note usage of (>>), not (>>=)
                        -- record gives Logger (), no result which needs capture
                        globToRegex' cs >>= \ds ->
                        return ('.':ds)
                        
globToRegex' ('*':cs) = do
    record "kleene star"        -- 'do' notation; no need to use (>>=) or (>>)
    ds <- globToRegex' cs
    return (".*" ++ ds)
    
globToRegex' ('[':'!':c:cs) = 
    record "Negative Character Class" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
    
globToRegex' ('[':c:cs) =
    record "Character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
    
globToRegex' ('[':_) =
    fail "unterminated character class"
    
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)
                      -- see below for an explanation on liftM2

escape :: Char -> Logger String 
escape c 
    | c `elem` regexChars = record "escape" >> return ['\\', c]
    | otherwise           = return [c]
    where regexChars = "\\+()^$.{}]|"

-- playing around with Logger 
str = "the string"
strLog = return str :: Logger String
lenStr = length str 
-- now, len strLog does not work, as length operates on String and not on Logger

lenStrLog = strLog >>= \s -> return (length s)
-- now, this will have Logger Int as return type and value of Logger 10
-- unwrap, find length and rewrap to Logger context
-- this is a generic operation, provided by liftM (liftM lifts a fn to a monad 
-- context)
lenStrLog2 = liftM length strLog
-- liftM defined in GHC.Base (Control.Monad)
{-
    liftM :: (Monad m) => (a -> b) -> m a -> m b
    liftM f x = x >>= \a -> return (f a)
-}

-- liftM helps in readability and conciseness of code 
-- as depicted below
charClass_wordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
    
charClass_wordy (c:cs) = 
    charClass_wordy cs >>= \ds ->
    return (c:ds)
    
-- instead, can be written simply as
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs 

-- liftM
-- often used in infix notation `liftM` 
-- Meaning: apply the pure function on the left to the result of monadic action
-- on the right
-- Apply the pure function on the left, to the value inside the monadic context  
-- on the right, wrap it in the monad and give back.    

-- liftM2 
-- liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
-- Execute m a, Execute m b 
-- Get these values and apply the function (a -> b -> c)
-- Wrap the result c in monadic context and give back 
{-
    liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
    liftM2 f m1 m2 = do
        v1 <- m1
        v2 <- m2
        return (f v1 v2)

    OR    
        
    liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
    liftM2 f m1 m2 =
        m1 >>= \a ->
        m2 >>= \b ->
        return (f a b)        
-}

-- MISCONCEPTIONS
{-  
    1. Monads are hard to understand                        NOT TRUE
    2. Monads are only useful for IO and imperative coding  NOT TRUE
    3. Monads are unique to Haskell                         NOT TRUE
    4. Monads are for controlling the order of evaluation   NOT TRUE
-}

newtype Logger a = Logger {execLogger :: (a, Log)}

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Functor Logger where 
    fmap f mv = do
        v <- mv
        return (f v)
        
instance Applicative Logger where 
    pure a = Logger (a, [])
    (<*>) mf mv = do
        f <- mf
        v <- mv
        return (f v)
    
instance Monad Logger where
    return a = Logger (a, [])
    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    m >>= k = let (a, w) = execLogger m
                  n = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)
    
