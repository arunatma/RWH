{-# LANGUAGE FlexibleContexts #-}
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
       eol
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

{-
    ghci> parseCSV ""
    Right []
    ghci> parseCSV "hi,hello\n"
    Right [["hi","hello"]]
    ghci> parseCSV "hi,"
    Left "(unknown)" (line 1, column 4):
    unexpected end of input
    expecting "," or "\n"
-}


{-  Handling empty cells and empty lines as well 
    ghci> parseCSV "Hi,\n\n,Hello\n"
    Right [["Hi",""],[""],["","Hello"]]
-}


-- Using sepBy and endBy combinators
-- endBy and sepBy: both take two functions (parsing content and parsing separator)
-- first: the content parsing 
-- second: the separator
-- endBy differs in the sense that it wants the separator to be the last entity

csvFile' = endBy line' eol'
line' = sepBy cell' (char ',')
cell' = many (noneOf ",\n")
eol' = char '\n'

parseCSV' :: String -> Either ParseError [[String]]
parseCSV' input = parse csvFile' "(unknown)" input

-- Choices and Errors
-- The way the new line is handled:
-- In Unix / Linux: "\n" alone
-- In Windows: "\r\n"
-- Macs (olden days): "\r"
-- May be we can add support for "\n\r" as well

-- We need to do:
-- Adjust "eol" function to recognize these characters 
-- Make "cell" function to ignore '\r' as well 

eol1 :: GenParser Char st String
eol1 = string "\n" <|> string "\n\r" 
-- (<|>) function tries matching the left, followed by the right
-- Here on left itself, the match succeeds, So, if there is a "\n\r", then 
-- '\r' is left dangling

{-
    ghci> parse eol1 "" "\n"
    Right "\n"
    ghci> parse eol1 "" "\n\r"             -- We wanted it to be Right "\n\r"
    Right "\n"
-}

-- To be ascertained that '\r' is dangling check out this with following code
-- pass on the remaining of eol to eof (ideally it should work, unless for the
-- dangling '\r')

{-
    ghci> parse (eol1 >> eof) "" "\n"
    Right ()
    ghci> parse (eol1 >> eof) "" "\n\r"
    Left (line 2, column 1):
    unexpected '\r'
    expecting end of input
-}

-- So, if we interchange the arguments of (<|>), it should work, right?
eol2 :: GenParser Char st String
eol2 = string "\n\r" <|> string "\n"
-- The left one checks for "\n\r".  So, if there is a '\n' and end of file, it 
-- fails.  So, it fixes the failing option of eol1 and breaks what was working!
{-
    ghci> parse (eol2 >> eof) "" "\n\r"
    Right ()
    ghci> parse (eol2 >> eof) "" "\n"
    Left (line 1, column 1):
    unexpected end of input
    expecting "\n\r"
-}

-- Ideally we wanted to have a look ahead, but string "\n\r" tried consuming 
-- '\n' only to encounter the EOF to throw an error

eol3 :: GenParser Char st String
eol3 = do char '\n'         
          char '\r' <|> return '\n'
          
        