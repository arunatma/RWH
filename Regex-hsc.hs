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
    
-- PCRE Constants
caseless       :: PCREOption
caseless       = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall         :: PCREOption
dotall         = PCREOption #const PCRE_DOTALL
    
-- Use hsc2hs 
-- C> hsc2hs Regex-hsc.hs to convert this .hsc file to .hs file     