-- support file for ch05.hs (uses ch05.hs)

module Main where

--  Any import directives must appear in a group at the beginning of a module
import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])

-- To link the file, to create exe:
-- ghc -o simple.exe Main.hs SimpleJSON.hs

