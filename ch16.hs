-- Real World Haskell
-- Chapter 16: Using Parsec
-- http://book.realworldhaskell.org/read/using-parsec.html

-- Using Parsec library
-- Parsec provides simple parsing functions and combinators to tie them up 
-- Performs both lexical analysis and parsing

import Text.ParserCombinators.Parsec

-- Simple CSV Parsing

-- the csv file contains 1 or more lines
csvFile :: GenParser Char st [[String]]
csvFile =
    do result <- many line
       eof
       return result
       
-- each line contains 1 or more cells       
line :: GenParser Char st [String]
line =
    do result <- cells
       eof
       return result
       
-- parsing the cells.  Parse first followed by the remaining
cells :: GenParser Char st [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first:next)
       
-- remaining cells: ends with comma indicating more 
remainingCells :: GenParser Char st [String]
remainingCells = 
    (char ',' >> cells)     -- found comma; then more cells coming 
    <|> (return [])         -- else give out an empty list
    
-- any cell can contain any character other than comma and EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

-- main parsing function
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

    