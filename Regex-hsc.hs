{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
-- Real World Haskell
-- Chapter 17: Foreign Function Interface
-- http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html

-- Part of Chapter 17

module Regex where

import Foreign
import Foreign.C.Types
import Foreign.C.String         -- for CString

#include "pcre.h"

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)
    
-- PCRE Constants (Compile options)
caseless       :: PCREOption
caseless       = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall         :: PCREOption
dotall         = PCREOption #const PCRE_DOTALL
    
-- Use hsc2hs 
-- C> hsc2hs Regex-hsc.hs to convert this .hsc file to .hs file     

-- PCRE Constants can either be defined manually like above
-- Or, we can use automatic binding as below
-- PCRE Compile Options
#{enum PCREOption, PCREOption
  , caseless1       = PCRE_CASELESS
  , dollar_endonly1 = PCRE_DOLLAR_ENDONLY
  , dotall1         = PCRE_DOTALL
  }

-- #enum has 3 fields
-- First field: PCREOption (how the haskell type to be treated as)
-- Second field: PCREOption (Optional constructor)
-- Third field: Name and constant association

-- Here we shall treat flags as abstract types (not as bitfields in C)
-- So, or-ing or and-ing of flags will be treated differently in Haskell

-- | Combine a list of options into a single option, using bitwise (.|.)
-- Take out the value from the PCREOption using unPCREOption and do bitwise OR
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

-- This is how pcre_compile is defined in pcre.h
{-
    pcre *pcre_compile(const char *pattern,
                       int options,
                       const char **errptr,
                       int *erroffset,
                       const unsigned char *tableptr);
-}
-- compiles a regular expression into an internal format 

-- The equivalent data types are defined in Foreign.C.Types
-- Argument descriptions:
-- 1st: Null '\0' terminated character pointer (for regex pattern)
--    : Haskell Equivalent is CString
-- 2nd: Options (int)
--    : We shall use PCREOption (runtime representation is CInt)
-- 3rd: a pointer to a C String - reference to any error msg during regex 
--      compilation 
--      Value of the pointer is modified during the fn execution to point to 
--      appropriate error type 
--    : Haskell Equivalent: Ptr CString 
--      (others in Haskell FFI: Ptr CInt for int, Ptr Word8 for unsigned char)

-- On Pointers
-- We have a nullPtr constant in Haskell to refer to null pointers 
-- Can compare for equality
-- Can cast from one pointer type to another 
-- Advance a pointer (in terms of bytes) using plusPtr
-- Modify the value pointed to using "poke"
-- Dereference a pointer using "peek"

-- The return type of *pcre_compile is an abstract pcre pointer
-- We can represent the same in Haskell in an abstract fashion

type PCRE = ()      -- means some unknown type
                    -- Ptr PCRE is a pointer to some unknown type
-- The chances are we may never dereference that pointer

-- the equivalent pcre_compile in haskell
foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile :: CString 
                   -> PCREOption
                   -> Ptr CString
                   -> Ptr CInt
                   -> Ptr Word8
                   -> IO (Ptr PCRE)
                   
-- safe and unsafe calls
-- safe call: inefficient; But Haskell can be safely called back from C
-- unsafe call: Far less overhead; C code must not call back into Haskell.
--       The instance of C calling Haskell is rare, so mostly we write 
--       "unsafe" call.

-- now PCRE is () pointer.  We can make it typed pointer to increase safety
data PCRE1      -- requires EmptyDataDecls language pragma
-- no data, we can only use them as pointers!

-- same is achieved using newtype recursive definition, without need of pragma 
newtype PCRE2 = PCRE (Ptr PCRE)
-- this can also be used only as pointer as it has no runtime representation

-- In C: Discipline of not dereferencing the PCRE pointer lies with programmer
-- In Haskell: The type system takes care of that 
-- If the code compiles, type checker has ensured that the PCRE pointer is not 
-- dereferenced

-- Memory Management: Using Garbage Collector
-- Memory needed for the PCRE allocated by library on the C side 
-- While the call returns to Haskell, the memory will still be in use and not 
-- deallocated.  We need to deallocate from within Haskell once it is no 
-- longer needed.

-- Use Haskell garbage collector finalizers and ForeignPtr type
-- No need to manually deallocate memory (which is originally allocated in C 
-- using malloc function).  Haskell's garbage collector does the job

-- We can use:
-- 1. ForeignPtr data type  and 
-- 2. newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
--      Takes two params: FinalizerPtr and a Ptr where memory to be deallocated
--      Returns a new managed pointer which will have the finalizer run 
--      once Haskell garbage collector decides the data is no more used 

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
        deriving (Eq, Ord, Show)
        
-- ForeignPtr because we need to manage the underlying memory allocated by C
-- ByteString is for the string representation of the regex that we compiled 
        
