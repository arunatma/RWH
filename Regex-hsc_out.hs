{-# LINE 1 "Regex-hsc.hs" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
{-# LINE 2 "Regex-hsc.hs" #-}
-- Real World Haskell
-- Chapter 17: Foreign Function Interface
-- http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html

-- Part of Chapter 17

module Regex where

import Foreign
import Foreign.C.Types
import Foreign.C.String         -- for CString
import Data.ByteString.Char8 (ByteString, useAsCString)
import System.IO.Unsafe


{-# LINE 17 "Regex-hsc.hs" #-}

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)
    
-- PCRE Constants (Compile options)
caseless       :: PCREOption
caseless       = PCREOption 1
{-# LINE 27 "Regex-hsc.hs" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 30 "Regex-hsc.hs" #-}

dotall         :: PCREOption
dotall         = PCREOption 4
{-# LINE 33 "Regex-hsc.hs" #-}
    
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

{-# LINE 45 "Regex-hsc.hs" #-}

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
        
        
-- Compilation: Should be referentially transparent (A 1-1 input output)
-- But the C compiles, stores the compiled output in some memory location and gives
-- different pointers each time (with the same input)
-- Failure can happen (if the pattern is invalid) - So provide for error using 
-- "Either" data type in Haskell

-- compile :: ByteString -> [PCREOption] -> Either String Regex

-- compile will call c_pcre_compile (that takes CString as input)
-- so we need to convert ByteString to CString
-- this is present in ByteString module 
{-
    useAsCString :: ByteString -> (CString -> IO a) -> IO a
-}
-- The second argument is an user-defined fn that takes the resulting converted
-- CString and produces an IO - Our useAsCString creates a CString which can 
-- be passed to C as a pointer 
-- A kind of pseudocode of using the useAsCString function
{-
    useAsCString str $ \cstr -> do
       ... operate on the C string
       ... return a result
       
    -- "$" operator above is to avoid the use of paranthesis
    -- Enhances readability
-}

-- Allocating local C data: the Storable class
-- pcre_compile needs some pointers and arrays to place the return values 
-- These data will be short-lived. Such short-lived C memory can be allocated 
-- in Haskell using "alloca"
{-
    alloca :: Storable a => (Ptr a -> IO b) -> IO b
-}
-- This allocation is similar to local stack variables in other languages
-- Takes a pointer of specific data type 'a'. Allocated memory gets released
-- when the scope exits.  
-- Data type allocated can be inferred from type information, based on the use.
-- Using the alloca
{-
    alloca $ \stringptr -> do
       ... call some Ptr CString function
       peek stringptr
-}
-- locally allocates a Ptr CString, applies the code block (which calls the C 
-- fn that modifies the pointer contents).  Dereference the pointer using 
-- "peek" function

-- So, allocate memory, get the pointer, give it to C, let it modify the 
-- contents, get back the pointer, get the value back from pointed memory 

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $ 
    useAsCString str $ \pattern -> do
        alloca $ \errptr    -> do
        alloca $ \erroffset -> do
            pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
            if pcre_ptr == nullPtr
                then do 
                    err <- peekCString =<< peek errptr
                    return (Left err)
                 else do
                    reg <- newForeignPtr finalizerFree pcre_ptr
                    return (Right (Regex reg str))
                    

-- unsafePerformIO - imported from System.IO.Unsafe
-- unsafePerformIO :: IO a -> a
-- Takes an IO value and converts to a pure one 
-- Dangerous Dangerous function!!
-- It can sidestep the safety guarantees of the Haskell type system!

-- Exists to bind the "known" referentially transparent C code to be bound to 
-- Haskell. "Haskell Compiler, I know what I am doing, this code is pure"
-- Using unsafePerformIO, we are asserting that the code is pure

-- There are four parts above:
-- 1. Marshalling Haskell data to C form    (useAsCString and alloca)
-- 2. Calling into the C library            (c_pcre_compile)
-- 3. Checking the return values            (test against nullPtr)
-- 4. Constructing a Haskell value from the results (dereference)

-- finalizerFree uses the C "free" function to deallocate memory

