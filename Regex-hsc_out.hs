{-# LINE 1 "Regex-hsc.hs" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex-hsc.hs" #-}
-- Real World Haskell
-- Chapter 17: Foreign Function Interface
-- http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html

-- Part of Chapter 17

module Regex where

import Foreign
import Foreign.C.Types
import Foreign.C.String         -- for CString


{-# LINE 15 "Regex-hsc.hs" #-}

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)
    
-- PCRE Constants (Compile options)
caseless       :: PCREOption
caseless       = PCREOption 1
{-# LINE 25 "Regex-hsc.hs" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 28 "Regex-hsc.hs" #-}

dotall         :: PCREOption
dotall         = PCREOption 4
{-# LINE 31 "Regex-hsc.hs" #-}
    
-- Use hsc2hs 
-- C> hsc2hs Regex-hsc.hs to convert this .hsc file to .hs file     

-- PCRE Constants can either be defined manually like above
-- Or, we can use automatic binding as below
-- PCRE Compile Options
caseless1        :: PCREOption
caseless1        = PCREOption 1
dollar_endonly1  :: PCREOption
dollar_endonly1  = PCREOption 32
dotall1          :: PCREOption
dotall1          = PCREOption 4

{-# LINE 43 "Regex-hsc.hs" #-}

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
                   

