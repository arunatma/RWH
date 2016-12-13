-- module to take JSON value and render it in the JSON format
-- PutJSON.hs

module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k, v) = show k ++ ": " ++ renderJValue v
          
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] =""
          values vs = intercalate ", " (map renderJValue vs)

-- this is an impure code, interacting with the real world          
putJValue :: JValue -> IO()
putJValue v = putStrLn (renderJValue v)

          
          