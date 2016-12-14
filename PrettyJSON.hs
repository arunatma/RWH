-- Pretty rendering of JSON

-- module header
module PrettyJSON
    (
      renderJValue
      -- The other functions in this module are help functions for renderJValue
      -- no need to expose those functions
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)
                 
-- text, double and string functions are provided by the Prettify module
renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary)  = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val

-- writing in point-free notation
-- point is not "."  point is the value.  See below, we have not given the 
-- values (arguments) on LHS and RHS
string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

-- "string" fn is equivalent of the below
pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))


-- enclose function wraps the Doc value with opening and closing character
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

-- The oneChar function escapes or renders an individual character
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
    
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])    
    
-- Definition of hexEscape    
-- To take care of unicode character - valid unicode is till 0x10ffff
smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff    
    
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

-- common abstraction for JSON Array and Object
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
                       
-- series function 
-- Inputs: 1. Start Character, 2. Close Character
-- 3. A function that knows how to convert 'a' to 'Doc' 
-- 4. A list of 'a' s to be converted
-- Output: 'Doc'
-- 'item' is the function that converts 'a' to 'Doc'
-- That is mapped on each of [a] to get [Doc]
-- Each of Doc is punctuated with ','
-- fsep converts [Doc] to Doc which is enclosed with open and close characters.

-- Appends the punctuation, except for the last 'Doc' in the list.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
