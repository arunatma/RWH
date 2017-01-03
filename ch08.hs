-- Chapter 08
-- Efficient File Processing, Regular Expressions, File Name Matching
-- http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

-- read text file of numbers and print the sum
readPrintSum = do
		contents <- getContents
		print (sumFile contents)
	where sumFile = sum . map read . words
	
-- readPrintSum works in ghci - enter the numbers one after another
-- When done press Ctrl-C, the sum of all the numbers entered will be printed.

-- In the above function, string is converted into numbers using 'read'
-- The processing will be slow, because of strings being involved

-- 'byteString' library has fast, cheap alternative to String datatype
-- 	byteString library has two modules
-- 		1. Data.ByteString		strict version, defines a datatype 'ByteString'
--		2. Data.ByteString.Lazy lazy version, also has the datatype 'ByteString'

-- Strict version used for random access and applications where memory is not a 
-- constraint. Lazy version works best for streaming large quantity of data. The
-- entire data is loaded as a single chunk and the entire chunk can be discarded
-- when not necessary.

-- Example: Function to determine whether a given file is ELF file (used for 
-- executable files in the unix systems)

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
	where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

-- 'take' is also available in Prelude.  That's the need of having qualified
-- import, so the specific take function can be referred as "L.take"
-- Since the function names are similar to what present in Prelude, always use
-- qualified import when working with the ByteStrings

-- Here Data.ByteString.Lazy.length and L.length identifies the same function
-- Similarly Prelude.sum or sum identifies the same function

isElfFile :: FilePath -> IO Bool
isElfFile path = do
	content <- L.readFile path
	return (hasElfMagic content)

-- L.readFile is the bytestring equivalent of readFile
-- Does the lazy read as and when data is needed. Reads in the chunk of 64KB


-- Text IO (since it uses byte-sized char, only ASCII is supported)
-- Helped by Data.ByteString.Char8 and Data.ByteString.Lazy.Char8 functions

{-
Both of the following are same
	ghci> putStr =<< readFile "prices.csv"
	ghci> readFile "prices.csv" >>= putStr
-}

-- Highest Closing Price from the "prices.csv" file
closing = readPrice . (!!4) . LC8.split ','

-- read number before and after the decimal point and convert to cents
readPrice :: LC8.ByteString -> Maybe Int
readPrice str =
    case LC8.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case LC8.readInt (LC8.tail rest) of
          Nothing           -> Nothing
          Just (cents,more) ->
            Just (dollars * 100 + cents)


highestClose = maximum . (Nothing:) . map closing . LC8.lines

highestCloseFrom path = do
    contents <- LC8.readFile path
    print (highestClose contents)

