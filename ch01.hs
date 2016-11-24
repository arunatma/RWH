-- {-# OPTIONS_GHC -Wall #-}

-- ch01.hs
-- Chapter 1. Getting Started
-- http://book.realworldhaskell.org/read/getting-started.html

-- prints the character count
main = interact characterCount
    where characterCount input = show (length input) ++ "\n"

-- word count
main = interact wordCount
    where wordCount input = show (length (words input)) ++ "\n"

-- line count
main = interact lineCount
    where lineCount input = show (length (lines input)) ++ "\n"
    