{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- Real World Haskell
-- Chapter 17: Foreign Function Interface
-- http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html

-- Part of Chapter 17

module Regex where

import Foreign
import Foreign.C.Types

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
  , dotall          = PCRE_DOTALL
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