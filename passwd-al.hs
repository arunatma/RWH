-- Part of Chapter 13
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

-- use "passwd" example file to test run the code
-- C:> runghc passwd-al.hs passwd 2

import Data.List
import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)

main = do
    args <- getArgs                         -- Load command line arguments
    
    when (length args /= 2) $ do
        putStrLn "Usage: passwd-al filename uid"
        exitFailure
        
    content <- readFile (args !! 0)         -- Lazy reading of the file
    
    let username = findByUID content (read (args !! 1))
    
    case username of
        Just x  -> putStrLn x
        Nothing -> putStrLn "Could not find the given UID"
        
findByUID :: String -> Integer -> Maybe String
findByUID content uid = lookup uid al
    where al = map parseline . lines $ content
        
-- to convert colon separated lines to fields
parseline :: String -> (Integer, String)
parseline input = (key, value)
    where key    = read (fields !! 2)
          value  = fields !! 0
          fields = split ':' input

-- split: Take a delimiter and a list; break the list into list of lists
split :: Eq a => a -> [a] -> [[a]]
split _ []      = [[]]
split delim str = first : rest
    where theTuple = span (/= delim) str
          first    = fst theTuple
          reminder = snd theTuple
          rest     = case reminder of
                        [] -> []
                        x  -> split delim (tail x)
                        
