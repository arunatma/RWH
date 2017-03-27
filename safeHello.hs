-- Real World Haskell
-- Part of Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html
-- safeHello.hs

import MonadHandle
import System.IO (IOMode (..))

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h
    