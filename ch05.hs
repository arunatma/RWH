{-# OPTIONS_GHC -Wall #-}

-- Chapter 4. Writing a library: working with JSON data
-- http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html

-- JSON - JavaScript Object Notation
-- Can have 4 basic types - strings, numbers, null and boolean
-- Two compound types - array and object

-- [-3.14, true, null, "a string"]                      array
--  {"numbers": [1,2,3,4,5], "useful": false}           object

-- The elements in the array can be of any types
-- The objects are combination of name value pairs, the names are always 
-- strings and the value can be one of the 4 basic types or 2 advanced types.

-- We are creating a module with name SimpleJSON
module SimpleJSON
    (
      JValue(..)
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where
    
-- Representing JSON data in Haskell    
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v            = v == JNull

