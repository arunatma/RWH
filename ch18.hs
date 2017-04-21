-- Real World Haskell
-- Chapter 18: Monad Transformers
-- http://book.realworldhaskell.org/read/monad-transformers.html

-- Adding failure handling capability to State Monad
-- Using monad transformers from mtl library

-- Monad transformers: Modifies the behaviour of the underlying monad
-- Most monads in mtl library have transformer equivalents
-- Naming convention: MonadT is the monad transformer for Monad 
-- StateT for State, WriterT for Writer
-- WriterT: write data when stacked on top of another monad

