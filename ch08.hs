-- Chapter 08
-- Efficient File Processing, Regular Expressions, File Name Matching
-- http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import Text.Regex.Posix

import GlobRegex        -- module written in this folder (part of Chapter 8)
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

-- File name matching

-- glob patterns (wild card or shell like patterns)
-- literals matches themselsves - 'foo' pattern matches 'foo' and nothing else
-- * matches anything foo* matches football, food etc
-- foo*.c matches anything that starts with 'foo' and ends with '.c'
-- ? matches single character. pic??.jpg matches pic01.jpg, pic02.jpg, picAB.jpg
-- [ ] character class - anything within the character class is valid (as opposed
-- to the '?' which says anything is valid
-- [! ] negated character class - match one character, but that should not be 
-- what provided withing the negated character class.
-- [0-9] match anything from 0 to 9

-- Haskell does not have a ready library to match the glob patterns.
-- Haskell has a great regular expression support.  And glob patterns can be 
-- thought of as a subset of regular expression.

-- regexp matching function (=~) the operator (actually a fn here) is borrowed
-- from Perl
-- (=~) TextToMatch Regex 
matchBool1 = "my left foot" =~ "foo" :: Bool            -- written in infix style
matchBool2 = "your right hand" =~ "bar" :: Bool
matchBool3 = "your right hand" =~ "(hand|foot)" :: Bool

-- RegexContext typeclass describes how target (the result of =~) should behave
-- RegexContext instances are defined for Bool, Int
-- The same matches seen above can be captured as Int as well.
-- If captured as Int, returns the total number of matches.
matchInt1 = "my left foot" =~ "foo" :: Int
matchInt2 = "your right hand" =~ "bar" :: Int
matchInt3 = "your right hand" =~ "(hand|foot)" :: Int
matchInt4 = "honorificabilitudinitatibus" =~ "[aeiou]" :: Int  -- 13

-- if the target is String, we get the actual matched word. (the first match)
matchString1 = "my left foot" =~ "foo" :: String
matchString2 = "your right hand" =~ "bar" :: String
matchString3 = "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: String

-- [String] - all matched strings
{-
matchStrList1 = "my left foot" =~ "foo" :: [String]
matchStrList2 = "your right hand" =~ "bar" :: [String]
matchStrList3 = "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: [String]
-}

-- (String, String, String) - string before, the matched string, string that 
-- follows, for the first match 
pat = "(foo[a-z]*bar|quux)"
matchTuple1 =
  "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: (String,String,String)  
matchTuple2 = "before foodiebar after" =~ pat :: (String,String,String)
matchTuple3 = "no match here" =~ pat :: (String,String,String)

-- (String,String,String,[String])
-- List of all groups in the pattern that matched.  
matchTupList1 = 
    "before foodiebar after" =~ pat :: (String,String,String,[String])
    
-- (Int, Int) - starting offset and the lenght for the first match
-- [(Int, Int)] - List of all offsets and lengths
matchIntTuple1 = "my left foot" =~ "foo" :: (Int, Int)
matchIntTuple2 = "before foodiebar after" =~ pat :: (Int,Int)
--matchIntTuple3 = "before foodiebar after quux" =~ pat :: [(Int,Int)]
--matchIntTuple4 = "i foobarbar a quux" =~ pat :: [(Int,Int)]

-- Somehow [String] and [(Int, Int)] is not working.

-- Text.Regex.Base module defines common API for regex
-- Text.Regex.Posix is one such implementation (with POSIX semantics)

-- Perl vs POSIX implementation.
-- Perl is leftmost match; POSIX is greediest match 
-- (foo|fo*) on foooooo will provide 'foo' as match in Perl; 'foooooo' in POSIX
-- Perl type regex matching has many features which are not there in POSIX.
-- Perl type regex is more uniform in syntax

-- Translating a glob pattern to a regular expression
-- see GlobRegex.hs

globMatch1 =  "foo.c" =~ globToRegex "f??.c" False :: Bool
globMatch2 =  "test.c" =~ globToRegex "t[ea]s*" False :: Bool
globMatch3 =  "task.c" =~ globToRegex "t[ea]s*" False :: Bool
globMatch4 =  "TASK.c" =~ globToRegex "t[ea]s*" False :: Bool   -- False
globMatch5 =  "TASK.c" =~ globToRegex "t[ea]s*" True :: Bool    -- True

-- Python has a 'fnmatch' module that has a 'translate' function.
-- 'translate' is Python equivalent of globToRegex
-- 'translate' is written in loop

{-

    globToRegex' (c:cs) = escape c ++ globToRegex' cs
    
    This is not tail recursion.
    The result of globToRegex' is passed to (++) function.
    For tail recursion, the result to be the result of recursive function call
-}
