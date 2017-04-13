{-# LANGUAGE ForeignFunctionInterface #-}
-- Real World Haskell
-- Chapter 17: Foreign Function Interface
-- http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html

import Foreign
import Foreign.C.Types

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

fastsin :: Double -> Double 
fastsin x = realToFrac (c_sin (realToFrac x))

main = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]

-- Running the file:
-- Method 1:
{-
    > ghci
    ghci> :l SimpleFFI.hs
    ghci> main 
-}

-- Method 2:
{-
    > ghci SimpleFFI.hs
    ghci> main
-}

-- Method 3:
{-
    > ghc -O --make SimpleFFI.hs
    > SimpleFFI.exe
-}