{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

-- Real World Haskell
-- Part of Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html
-- MonadHandle.hs

-- Using the type classes

-- Let us say we used HandleIO monad for the IO functions 
-- If we want to change it to some other monad, we need to change all the places 
-- where HandleIO is used

-- Better to create a type class that mandates an interface to be used for 
-- a monad that handles file operations
module MonadHandle (MonadHandle (..)) where 

import System.IO (IOMode (..))

class Monad m => MonadHandle h m | m -> h where
    openFile :: FilePath -> IOMode -> m h
    hPutStr :: h -> String -> m ()
    hClose :: h -> m ()
    hGetContents :: h -> m String
    
    hPutStrLn :: h -> String -> m ()
    hPutStrLn h s = hPutStr h s >> hPutStr h "\n"
    
-- FunctionalDependency above states that for any instance of MonadHandle, there
-- is exactly one handle type that can be used

-- We can make IO monad an instance of MonadHandle type class