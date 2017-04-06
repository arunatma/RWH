{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- ApplicativeParsec.hs

-- Real World Haskell
-- Part of Chapter 16: Using Parsec
-- http://book.realworldhaskell.org/read/using-parsec.html

module ApplicativeParsec 
    (
        module Control.Applicative
    ,   module Text.ParserCombinators.Parsec
    ) where 
    
import Control.Applicative
import Control.Monad (MonadPlus (..), ap)

-- hiding a few fns that are to be provided by this module 
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))    

instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap
    
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus
    
-- This file does not compile, as these functions are already defined.
-- While RWH being written, these would not have been defined in standard 
-- libraries.    