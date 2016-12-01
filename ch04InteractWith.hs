-- file: ch04InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)
import Data.Char

-- Define the function "interactWith"
-- "interactWith" takes a processing "function". apply the function over input 
-- text; get the result and writes the result into output file.
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

-- main gets called and it calls mainWith with myFunction, both of which 
-- are defined using the "where" clause  
main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _              -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        -- myFunction = id
        -- myFunction = map toUpper 
        myFunction = fixLines
        
-- Method 1: Compile into an "exe" and run
    -- > ghc --make ch04InteractWith.hs
    -- > ./ch04InteractWith.exe

-- Method 2: Run from commandline without creating "exe"
    -- > runhaskell ch04InteractWith.hs
    
    
-- splitLines - taking care of both \r and \n
splitLines :: String -> [String]
splitLines [] = []
splitLines cs = 
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'                

-- fixLines - remove \r\n or \n from the string; 
fixLines :: String -> String
fixLines input = unlines (splitLines input)    
