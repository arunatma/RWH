-- GlobRegex.hs
-- part of chapter 8: Efficient file handling and regular expressions
-- http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html


module GlobRegex 
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))
import Data.Char (toUpper)
import Control.Applicative ((<$>), (<*>))

globToRegex :: String -> Bool -> String
-- anchor the regex to start matching from beginning to end of string 
-- start anchor: '^' and end anchor: '$'
globToRegex cs ic = '^' : globToRegex' (upperOnNeed cs ic) ++ "$"

upperOnNeed :: String -> Bool -> String
upperOnNeed cs ic
    | ic        = upperCase cs
    | otherwise = cs
    
upperCase :: String -> String
upperCase ""        = ""
upperCase (c:cs)    = (toUpper c) : upperCase cs

globToRegex' :: String -> String
globToRegex' ""             = ""
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['   : c : charClass cs
globToRegex' ('[':_)        = error "Unterminated Character Class"
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

-- these are the escape characters; do not form part of regex 
escape :: Char -> String
escape c 
    | c `elem` regexChars = '\\' : [c]
    | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

-- the start of character class is matched in globToRegex' function.
-- here the ending ']' is matched and the remaining str is passed to 
-- globToRegex' again.
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

-- to match the file names
matchesGlob :: FilePath -> String -> Bool -> Bool
(name `matchesGlob` pat) ignoreCase = name =~ globToRegex pat ignoreCase

-- making use of Either data type to catch errors 
-- Helps in graceful failure instead of throwing 'error' and terminating
-- Suffixed with GF - 'Graceful Failure'
type GlobError = String
globToRegexGF :: String -> Bool -> Either GlobError String
globToRegexGF cs ic = (++) <$> prefixed <*> Right "$"
    where regexValue = globToRegexGF' (upperOnNeed cs ic)
          prefixed = (:) <$> Right '^' <*> regexValue
    
globToRegexGF' :: String -> Either GlobError String
globToRegexGF' ""             = Right ""
globToRegexGF' ('*':cs)       = (++) <$> Right (".*") <*> globToRegexGF' cs
globToRegexGF' ('?':cs)       = (:)  <$> Right ('.')  <*> globToRegexGF' cs
globToRegexGF' ('[':'!':c:cs) = Right ("[^" ++ c : charClass cs)
globToRegexGF' ('[':c:cs)     = Right ('['   : c : charClass cs)
globToRegexGF' ('[':_)        = Left "Unterminated Character Class"
globToRegexGF' (c:cs)         = (++) <$> Right (escape c) <*> globToRegexGF' cs

-- Exercises
-- 1. Write a version of globToRegex that uses the type signature above
--    Done - see above.
-- 2. Modify the type signature of namesMatching so that it encodes the 
--    possibility of a bad pattern by using the rewritten globToRegexGF fn
--    (Yet to be done)

