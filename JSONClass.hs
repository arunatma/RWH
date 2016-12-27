{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JSONClass where

import SimpleJSON
import Control.Arrow (second)

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right
    
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

-- Using type synonym instances 
-- TypeSynonymInstances language extension (this did not work)
-- So used FlexibleInstances
-- Otherwise we have to write [Char]
instance JSON String where
    toJValue               = JString
    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id

-- We can add new instances anywhere - in any file.
-- "open world assumption" - an instance can be written for any data type.

-- Writing an instance for a list (JSON array)
instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined

-- Writing an instance for the key-value pair
instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
    
-- New instances for JAry and JObj
jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryToJValue :: (JSON a) => JAry a -> JValue


-- Helper functions for jaryFromJValue and jaryToJValue
listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

-- So, jaryToJValue can be written as:
jaryToJValue = JArray . JAry . map toJValue . fromJAry

-- Converting from JValue to JAry is a little tricky!
jaryFromJValue (JArray (JAry a)) =
    whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

-- This uses two custom functions "whenRight" and "mapEithers"

-- whenRight
-- Takes a function and a parameter. Applies the function on the parameter if
-- the parameter is created using "Right" data constructor
whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

-- mapEithers
-- Map with the given function, if all values are created using "Right"
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue
    
instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj

    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
      where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"
    