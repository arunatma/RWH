{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JSONClass where

import SimpleJSON

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

    