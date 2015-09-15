-- Example Code in Chapter 13 - Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html
-- Parses a text file
-- On Unix/Linux machines, there is a file called /etc/passwd that stores 
-- usernames, UIDs, home directories, and various other data. We will write a 
-- program that parses such a file, creates an association list, and lets the 
-- user look up a username by giving a UID.


-- To run from command line
-- runhaskell 13PasswdAL.hs filename UID
-- Command Line arguments: Total 2 - first is file to process, second is UID
import Data.List
import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)

main = do
    args <- getArgs                         -- Load CommandLine Arguments
    when (length args /= 2) $ do
        putStrLn "Syntax: 13PasswdAL filename uid"
        exitFailure
        
    content <- readFile (args !! 0)         -- Read the file when required
    let username = findByUID content (read (args !! 1))
    case username of
        Just x -> putStrLn x
        Nothing -> putStrLn "Could not find the UID in the file"

-- Given the entire content of file and the UID, returns the username        
findByUID :: String -> Integer -> Maybe String
findByUID content uid = 
    let al = map parseLine . lines $ content
        in lookup uid al
    
-- Convert a colon separated line to fields
parseLine :: String -> (Integer, String)
parseLine input =
    let fields = split ':' input
        in (read (fields !! 2), (fields !! 0))
        
-- Split: Takes up a delimiter and a string; splits based on delimiter
split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]                           -- List of Empty Lists
split delim str = 
    let (pref, suff) = span (/= delim) str
    in pref : case suff of
                [] -> []
                x  -> split delim (tail x)
      
