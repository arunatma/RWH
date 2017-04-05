{-# LANGUAGE FlexibleContexts #-}
-- Real World Haskell
-- Chapter 16: Using Parsec
-- http://book.realworldhaskell.org/read/using-parsec.html

-- Part of Chapter 16
-- Extended Example: Full CSV Parser

import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

-- To get something within quotes like "....."
quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content
       
-- Get anything other than a ("). If ("), then see whether you have two (")s
-- "try" usually on the left side of <|>, but here it is on the right
-- At end of the cell, there is a quote, so, noneOf will fail
-- double quote testing will fail looking for two quotes one after another, as 
-- now there is only a single quote followed by a comma. "try" avoids the fail
-- as the character is not consumed looking for the test
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')
    
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"    
    
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = 
    do c <- getContents 
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r

{- Commas allowed within quoted char! 
    ghci> parseCSV "\"This, is, one, big, cell\"\n"
    Right [["This, is, one, big, cell"]]
    
    Now, non terminated quote!
    
    ghci> parseCSV "\"Cell without an end\n"
    Left "(unknown)" (line 2, column 1):
    unexpected end of input
    expecting "\"\"" or quote at end of cell    
    
    Quotes within quotes within quotes!
    
    ghci> parseCSV "\"Shirt with \"\"Haskell\"\" text\",20\n"
    Right [["Shirt with \"Haskell\" text","20"]]
-}            

{- Running as an independent application

    C> runhaskell csvParser < parserTest.csv
        ["Product","Price"]
        ["O'Reilly Socks","10"]
        ["Shirt with \"Haskell\" text","20"]
        ["Shirt, \"O'Reilly\" version","20"]
        ["Haskell Caps","15"]
    
-}