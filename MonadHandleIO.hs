{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

-- Real World Haskell
-- Part of Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html
-- MonadHandleIO.hs

import MonadHandle
import qualified System.IO 

import System.IO (IOMode (..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))

import System.Directory (removeFile)

instance MonadHandle System.IO.Handle IO where
    openFile = System.IO.openFile
    hPutStr = System.IO.hPutStr
    hClose = System.IO.hClose
    hGetContents = System.IO.hGetContents
    hPutStrLn = System.IO.hPutStrLn
    
