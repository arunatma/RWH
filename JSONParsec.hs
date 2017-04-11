{-# LANGUAGE FlexibleContexts #-}

-- JSONParsec.hs

-- Real World Haskell
-- Part of Chapter 16: Using Parsec
-- http://book.realworldhaskell.org/read/using-parsec.html

-- JSON follows the specification in RFC 4627 
-- (https://www.ietf.org/rfc/rfc4627.txt)

import Text.ParserCombinators.Parsec
import SimpleJSON
import Numeric

-- At the top level, the JSON must either be an object or an array
p_text :: CharParser () JValue
p_text = spaces *> text
        <?> "JSON text"
        where text =   JObject <$> p_object
                   <|> JArray  <$> p_array
                   
-- Object and Array are structurally similar
-- Opening marker, series of objects separated by commas, closing marker 
-- capturing that abstraction here, separately

p_series :: Char -> CharParser() a -> Char -> CharParser () [a]
p_series left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)
            
-- use of <* is to skip over the spaces if any after the recognized tokens 
-- use of p_series to parse an array
p_array :: CharParser () (JAry JValue)
p_array = JAry <$> p_series '[' p_value ']'

-- use of p_series to parse a JSON object (just have to handle key value pairs)
p_object :: CharParser () (JObj JValue)
p_object = JObj <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value
    
-- parsing an individual object is about calling an existing parser and wrapping
-- it in appropriate JValue constructor
p_value :: CharParser () JValue
p_value = value <* spaces
    where value = JString <$> p_string
              <|> JNumber <$> p_number
              <|> JObject <$> p_object
              <|> JArray <$> p_array
              <|> JBool <$> p_bool
              <|> JNull <$ string "null"
              <?> "JSON value"
              
p_bool :: CharParser () Bool
p_bool = True <$ string "true" 
     <|> False <$ string "false"
     
-- p_value written using "choice" combinator function
p_value_choice = value <* spaces
    where value = choice [ JString <$> p_string
                       , JNumber <$> p_number
                       , JObject <$> p_object
                       , JArray <$> p_array
                       , JBool <$> p_bool
                       , JNull <$ string "null"
                       ]
                <?> "JSON value"

-- p_number, the number parser
p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> 0 <$ setInput s        -- 'empty' does not work!
                
-- getInput consumes from Parsec stream
-- readFloat - from Numeric library - reads unsigned float 
-- readSigned - deals with the unsigned float, to make it work for signed ones
-- The return value is set as the input to the Parsec stream using setInput


p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (p_escape <|> p_unicode) 
              <|> satisfy (`notElem` "\"\\")
              
p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

-- JSON lets unicode encoding with '\u' followed by 4 hex digits

p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x
              
{-
  ghci> parseTest p_text "{  \"firstName\": \"John\",  \"lastName\": \"Smith\"}"
  JObject (JObj {fromJObj = [("firstName",JString "John"),("lastName", 
  JString "Smith")]})
  
  ghci> (readFile "sample.json") >>= (parseTest p_text)
-}