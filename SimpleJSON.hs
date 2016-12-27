-- {-# OPTIONS_GHC -Wall #-}

-- Chapter 5. Writing a library: working with JSON data
-- http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html

-- JSON - JavaScript Object Notation
-- Can have 4 basic types - strings, numbers, null and boolean
-- Two compound types - array and object

-- [-3.14, true, null, "a string"]                      array
--  {"numbers": [1,2,3,4,5], "useful": false}           object

-- The elements in the array can be of any types
-- The objects are combination of name value pairs, the names are always 
-- strings and the value can be one of the 4 basic types or 2 advanced types.

-- We are creating a module with name (SimpleJSON in the book)
-- A source file must have the same base name (the component before the suffix)
-- as the name of the module it contains

module SimpleJSON
    (
      JValue(..)
    , JAry (..)
    , JObj (..)
    , getString
    , getInt
    , getDouble
    , getBool
    , isNull
    ) where

-- Omit () to export all the functions: "module ch05 where"
-- To export nothing, give an empty (): "module ch05 () where"


-- For JSON array
newtype JAry a = JAry {
        fromJAry :: [a]
    } deriving (Eq, Ord, Show)

-- instead of exporting the JAry data constructor, define a function jary 
-- that can be exported in the module.
-- This gives freedom to change the implementation of JAry later by the module
-- author. Otherwise, the users may depend on data constructor and changing 
-- that may break user's code.

-- So, in general, only export the following
-- Type Constructor, Construction Function (jary) and Deconstruction function 
-- (fromJAry). Never export the data constructor (JAry)
-- (but such restricted export is not done in this module! )
jary :: [a] -> JAry a
jary = JAry
    
-- For JSON object
newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)
    
-- Representing JSON data in Haskell    
-- After the createion of JSONClass.hs and the need of using "newtype" instead
-- of overlapping instances, JObject and JArray are newly defined.            
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was [(String, JValue)]
            | JArray (JAry JValue)    -- was [JValue]
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

{-
-- Used for JObject [(String, JValue)] and not for JObject (JObj JValue)
getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

-- Used for JArray [JValue] and not for JArray (JAry JValue)
getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing
-}

isNull :: JValue -> Bool
isNull v            = v == JNull

-- Compiling Haskell Source
-- ghc -c ch05.hs 
-- -c: generate only the object code (and not the executable)
-- ghc ch05.hs
-- tries generating executable. Fails! As, we don't have "main" function yet.
-- Output of compilation:
-- ghc.hi (haskell interface file, info on names exported)
-- ghc.o  (object file containing machine code of the functions)

-- Use this file, with Main.hs, where the main function is defined
-- To link the main file, to create exe:
-- ghc -o simple.exe Main.hs SimpleJSON.hs

