-- GlobRegex.hs
-- part of chapter 8: Efficient file handling and regular expressions

module GlobRegex 
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))
import Data.Char (toUpper)

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
