-- Example Code in Chapter 13 - Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html
-- Parses a text file; Uses Data.Map
-- On Unix/Linux machines, there is a file called /etc/passwd that stores 
-- usernames, UIDs, home directories, and various other data. We will write a 
-- program that parses such a file, creates an association list, and lets the 
-- user look up a username by giving a UID.


-- To run from command line
-- runhaskell 13PasswdMap.hs filename

import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Exit
import Control.Monad (when)

-- This data type represent fields in a POSIX /etc/passwd file
data PasswdEntry = PasswdEntry {
    userName :: String,
    password :: String,
    uid :: Integer,
    gid :: Integer,
    gecos :: String,
    homeDir :: String,
    shell :: String} deriving (Eq, Ord)
    
-- An instance of Show type class to PasswdEntry
-- This defines how we get data to a 'PasswdEntry'
instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s" 
                (userName pe) (password pe) (uid pe) (gid pe)
                (gecos pe) (homeDir pe) (shell pe)
             
-- An instance of Read type class to PasswdEntry             
-- Converting data back out of a 'PasswdEntry'
instance Read PasswdEntry where
    readsPrec _ value = case split ':' value of
        [f1, f2, f3, f4, f5, f6, f7] -> 
            [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
            -- Use positional fields; Use 'read' to convert numeric 
            -- fields to Integers
        x -> error $ "Invalid number of fields in input: " ++ show x

-- define the split function
split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim str = 
    let (pref, suff) = span (/=delim) str in
    pref : case suff of
        [] -> []
        x  -> split delim (tail x)
        
-- Convenience Aliases
-- Two map type synonyms -- mapping usins username and uid
type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

-- inputToMaps: Converts user data to two maps - indexed by 1. uid 2. user
inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp = (uidmap, usermap) where
    uidmap  = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
    usermap = Map.fromList . map (\pe -> (userName pe, pe)) $ entries
    entries = map read (lines inp)      -- Convert input string to list of [pe]
    
-- To run from command line
-- runhaskell 13PasswdMap.hs filename    
main = do
    -- Load command line arguments
    args <- getArgs
    -- Abort when the input number of arguments is incorrect
    when (length args /= 1) $ do
        putStrLn "Syntax: 13PasswdMap.hs filename"
        exitFailure
    -- Read the file lazily
    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps
    
mainMenu maps@(uidmap, usermap) = do 
    putStr optionText
    hFlush stdout
    sel <- getLine
    -- For every option other than '4', call mainMenu recursively
    case sel of
        "1" -> lookupUserName >> mainMenu maps
        "2" -> lookupUID >> mainMenu maps
        "3" -> displayFile >> mainMenu maps
        "4" -> return()
        _   -> putStrLn "Invalid Selection" >> mainMenu maps
    where
    lookupUserName = do
        putStrLn "Username: "
        username <- getLine
        case Map.lookup username usermap of
            Nothing -> putStrLn "Not Found!"
            Just x -> print x
            
    lookupUID = do
        putStrLn "UID: "
        uidstring <- getLine
        case Map.lookup (read uidstring) uidmap of
            Nothing -> putStrLn "Not Found!"
            Just x -> print x
            
    displayFile =
        putStr . unlines . map (show . snd) . Map.toList $ uidmap
        
    optionText = 
        "\npasswdmap options:\n\
           \\n\
           \1   Look up a user name\n\
           \2   Look up a UID\n\
           \3   Display entire file\n\
           \4   Quit\n\n\
           \Your selection: "
     