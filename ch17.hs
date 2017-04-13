{-# LANGUAGE ForeignFunctionInterface #-}
-- Real World Haskell
-- Chapter 17: Foreign Function Interface
-- http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html

-- Interfacing with C - the FFI
-- Foreign Function Interface - helps use the libraries written in other 
-- languages

-- Objective: PCRE - Perl Compatible Regular Expression library to be taken 
-- from C and used in Haskell in a functional way.

-- Addendum to Haskell 98 on FFI available at:
-- http://www.cse.unsw.edu.au/~chak/haskell/ffi/ffi/ffi.html
-- This helps in accessing hardware or OS directly
-- Helps the key performance pieces to be directly written in C and accessing 
-- C functions from Haskell.

-- Include the language pragma (ForeignFunctionInterface)

import Foreign
import Foreign.C.Types
-- Others: Foreign.C.String, Foreign.Ptr, Foreign.Marshall.Array
-- Above FFI link on cs.unsw.edu will give more details.

-- Now, we can call C functions 
-- Just know these:
--      1. Name of C funtion
--      2. Its data type
--      3. Associated header file 

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble
-- when csin is called, actual sin function is called from the C library
-- Haskell runtime passes control to C, gets the result and wraps the result 
-- in CDouble data type

-- raw C function in math.h has this following type declaration 
-- double sin (double x);
-- This needs to be translated to appropriate Haskell data type 
-- double --> CDouble in Haskell
-- Need to make sure the data types are translated correctly.  Otherwise it 
-- may cause a runtime crass. QuickCheck does not capture the errors here!
-- Other primitive C types:
-- CChar, CUChar, CInt, CUInt, CLong, CULong, CSize, CFloat, CDouble
    
-- Need to be careful of side effects
-- sin function of C here is referentially transparent (but not every C fn!)
-- "Pure, threadsafe C code, while rare, is a valuable commodity."

-- Common in C functions to have the actual work done via side effects 
-- The return values for such fns may be status or void
-- Handle those using IO monad in Haskell (IO CDouble)

-- Next step: convert C types to native Haskell types 
fastsin :: Double -> Double 
fastsin x = realToFrac (c_sin (realToFrac x))

-- using the C sin function on a Haskell list
listSin = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]

