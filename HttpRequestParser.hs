{-# LANGUAGE FlexibleContexts #-}

-- HttpRequestParser.hs

-- Real World Haskell
-- Part of Chapter 16: Using Parsec
-- http://book.realworldhaskell.org/read/using-parsec.html

-- Basic parser for HTTP Requests
module HttpRequestParser 
    (
      HttpRequest (..)
    , Method (..)
    , p_request
    , p_headers
    ) where 
    
import Text.ParserCombinators.Parsec
import Numeric (readHex)
import Control.Monad (liftM4)
import Control.Applicative (liftA2)

-- HTTP request consist of 
--  1. A method
--  2. An identifier
--  3. A series of headers
--  4. A body (optional)

-- 6 method types are specified in HTTP specification
-- Focus here on only: GET and POST
-- POST has a body, and GET has none

data Method = Get | Post deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest {
      reqMethod :: Method
    , reqURL :: String
    , reqHeaders :: [(String, String)]
    , reqBody :: Maybe String
    } deriving (Eq, Show)
    
-- Applicative style parser
p_request :: CharParser () HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
    where q name ctor body = liftM4 HttpRequest req url p_headers body 
            where req = ctor <$ string name <* char ' '
                  url = optional (char '/') *>
                        manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
                        <* crlf

-- try fn here holds on to the input, in case it needs it (so an alternative 
-- parser can be used).  This is called BACKTRACKING
-- Remember the use of "try" for looking ahead?
-- So, usage of "try" is expensive.

parser1 :: CharParser st String 
parser1 = try (string "HTTP") <|> (string "HTML")

-- the same parser can be written differently without usage of "try"
parser2 :: CharParser st String
parser2 = (++) <$> (string "HT") <*> (string "TP" <|> string "ML")

{- better error message as well, for parser2

    ghci> parseTest parser1 "HTTP"      -- "HTTP"
    ghci> parseTest parser1 "HTML"      -- "HTML"
    ghci> parseTest parser1 "HTXY"
    parse error at (line 1, column 1):
    unexpected "X"
    expecting "HTML"    
    
    ghci> parseTest parser2 "HTTP"      -- "HTTP"
    ghci> parseTest parser2 "HTML"      -- "HTML"
    ghci> parseTest parser2 "HTXY"
    parse error at (line 1, column 3):
    unexpected "X"
    expecting "TP" or "ML"    
-}

-- ok, now coming to parse the headers 

p_headers :: CharParser st [(String, String)]
p_headers = header `manyTill` crlf
    where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
          contents = liftA2 (++) (many1 notEOL <* crlf)
                                 (continuation <|> pure [])
          continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
          fieldName = (:) <$> letter <*> many fieldChar
          fieldChar = letter <|> digit <|> oneOf "-_"
          
crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"
