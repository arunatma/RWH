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

eol3 :: GenParser Char st Char
eol3 = 
     do char '\n'         
        char '\r' <|> return '\n'
{-
    ghci> parse (eol3 >> eof) "" "\n\r"
    Right ()
    ghci> parse (eol3 >> eof) "" "\n"
    Right ()
    ghci> parse (eol3 >> eof) "" "\r\n"
    Left (line 1, column 1):
    unexpected "\r"
    expecting "\n"
-}
-- Now "\n" and "\n\r" works; But still "\r\n" does not!!

-- LookAhead:
-- Look ahead without consumint the characters using the "try" function 
-- Takes a function and a parser as arguments.
-- Applies the parser; If it fails, then behaves as if it has not consumed!
-- if "try" applied on left side of (<|>), then, even if it fails after 
-- consuming some input, Parsec tries the right side option of (<|>), assuming 
-- no input is consumed.

csvFile4 = endBy line4 eol4
line4 = sepBy cell4 (char ',')
cell4 = many (noneOf ",\n\r")

eol4 =   try (string "\n\r")
     <|> try (string "\r\n")       -- If succeeds, then the characters consumed!
     <|> string "\n"
     <|> string "\r"
     
parseCSV4 :: String -> Either ParseError [[String]]
parseCSV4 input = parse csvFile4 "(unknown)" input

-- Now all combinations work!
{-
    ghci> parse (eol4 >> eof) "" "\n\r"
    Right ()
    ghci> parse (eol4 >> eof) "" "\r\n"
    Right ()
    ghci> parse (eol4 >> eof) "" "\n"
    Right ()
    ghci> parse (eol4 >> eof) "" "\r"
    Right ()
-}

{- Combining all, in parseCSV4 function 
    ghci> parseCSV4 "line1\r\nline2\nline3\n\rline4\rline5\n"
    Right [["line1"],["line2"],["line3"],["line4"],["line5"]]
-}

-- The following cases are taken care of
-- 1. CSV file of 0 or more lines ending with end of line character
-- 2. A line containing 1 or more cells separated by comma
-- 3. A cell containing 0 or more characters (except command and EOL)
-- 4. End of Line character being one of the 4 scenarios.

-- Error Handling:
-- Providing custom error messages via Parsec
csvFile5 = endBy line5 eol5
line5 = sepBy cell5 (char ',')
cell5 = many (noneOf ",\n\r")

eol5 =   try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\n"
     <|> string "\r"
     <|> fail "Could not find EOL"
     
parseCSV5 :: String -> Either ParseError [[String]]
parseCSV5 input = parse csvFile5 "(unknown)" input
     
{-  Here is the difference between the two functions

    ghci> parseCSV4 "line1"
    Left "(unknown)" (line 1, column 6):
    unexpected end of input
    expecting ",", "\n\r", "\r\n", "\n" or "\r"

    ghci> parseCSV5 "line1"
    Left "(unknown)" (line 1, column 6):
    unexpected end of input
    expecting ",", "\n\r", "\r\n", "\n" or "\r"
    Could not find EOL

-}     

-- Still it prints out all the options present in eol5
-- Usage of (<?>) - Prints only the message and not the parser trials
-- Here "fail" function is not used 

csvFile6 = endBy line6 eol6
line6 = sepBy cell6 (char ',')
cell6 = many (noneOf ",\n\r")

eol6 =   try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\n"
     <|> string "\r"
     <?> "End of Line"
     
parseCSV6 :: String -> Either ParseError [[String]]
parseCSV6 input = parse csvFile6 "(unknown)" input

{-
    *Main> parseCSV6 "line1"
    Left "(unknown)" (line 1, column 6):
    unexpected end of input
    expecting "," or End of Line
-}

-- See csvParser.hs for the full CSV Parser (Extended Example)