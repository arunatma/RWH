{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses  #-}

-- Real World Haskell
-- Part of Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html
-- WriterIO.hs

import MonadHandle
import Control.Monad.Writer
import System.IO (IOMode (..), Handle)
import SafeHello

data Event = Open FilePath IOMode
           | Put String String 
           | Close String
           | GetContents String
           deriving (Show)
           
-- There is a Writer monad in Control.Monad.Writer (mtl package) which 
-- provides the logger facility (remember the Logger monad we developed?)
newtype WriterIO a = W {runW :: Writer [Event] a}
    deriving (Functor, Applicative, Monad, MonadWriter [Event])
    
-- execution function - should be composed of runW and runWriter
runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

--writeSafe = runWriterIO (safeHello "foo")


-- check this: Unable to make this work 
{-
    ghci> runWriterIO (safeHello "foo")
    ((),[Open "foo" WriteMode,Put "foo" "hello world",Put "foo" "\n",Close "foo"])
-}