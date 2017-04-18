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


{-# LINE 14 "Regex-hsc.hs" #-}

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)
    
-- PCRE Constants (Compile options)
caseless       :: PCREOption
caseless       = PCREOption 1
{-# LINE 24 "Regex-hsc.hs" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 27 "Regex-hsc.hs" #-}

dotall         :: PCREOption
dotall         = PCREOption 4
{-# LINE 30 "Regex-hsc.hs" #-}
    
-- Use hsc2hs 
-- C> hsc2hs Regex-hsc.hs to convert this .hsc file to .hs file     

-- PCRE Constants can either be defined manually like above
-- Or, we can use automatic binding as below
-- PCRE Compile Options
caseless1        :: PCREOption
caseless1        = PCREOption 1
dollar_endonly1  :: PCREOption
dollar_endonly1  = PCREOption 32
dotall           :: PCREOption
dotall           = PCREOption 4

{-# LINE 42 "Regex-hsc.hs" #-}
