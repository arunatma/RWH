-- Part of Chapter 13
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

-- use "passwd" example file to test run the code
-- C:> runghc passwdmap.hs passwd

import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Exit
import Control.Monad (when)

-- Fields in a /etc/passwd file
data PasswdEntry = PasswdEntry {
    userName :: String,
    password :: String,
    uid :: Integer,
    gid :: Integer,
    gecos :: String,
    homeDir :: String,
    shell :: String } deriving (Eq, Ord)
    
-- A specialized Show instance to display how we want
instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s"
                (userName pe) (password pe) (uid pe) (gid pe)
                (gecos pe) (homeDir pe) (shell pe)
                
-- Reading a PasswdEntry (given in the Show format above)
instance Read PasswdEntry where
    readsPrec _ value =
        case split ':' value of
            [f1, f2, f3, f4, f5, f6, f7] -> 
                [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
            x -> error $ "Invalid number of fields in input: " ++ show x
        where
        -- same function as defined in passwd-al.hs 
        split :: Eq a => a -> [a] -> [[a]]  -- break up a list to list of lists
        split _ []      = [[]]
        split delim str = first : rest
            where theTuple = span (/= delim) str
                  first    = fst theTuple
                  reminder = snd theTuple
                  rest     = case reminder of
                                [] -> []
                                x  -> split delim (tail x)
        
-- Convenience - typecasting maps
-- uid based map and username based map
type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

-- convert the input data to maps 
inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp = (uidmap, usermap)
    where uidmap = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
          usermap = Map.fromList . map (\pe -> (userName pe, pe)) $ entries
          entries = map read (lines inp)
          
main = do
    args <- getArgs         -- command line arguments
    
    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap.hs filename"
        exitFailure
        
    -- lazy read file
    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps
    
mainMenu maps@(uidmap, usermap) = do
    putStr optionText
    hFlush stdout
    sel <- getLine
    case sel of
        "1" -> lookupUserName >> mainMenu maps
        "2" -> lookupUID >> mainMenu maps
        "3" -> displayFile >> mainMenu maps
        "4" -> return ()
        _   -> putStrLn "Invalid Selection" >> mainMenu maps
        
    where
    lookupUserName = do
        putStrLn "Username: "
        username <- getLine
        case Map.lookup username usermap of
            Nothing -> putStrLn "Not Found."
            Just x -> print x
        
    lookupUID = do
        putStrLn "UID: "
        uidstring <- getLine
        case Map.lookup (read uidstring) uidmap of
            Nothing -> putStrLn "Not Found."
            Just x -> print x
            
    displayFile = putStrLn . unlines . map (show . snd) . Map.toList $ uidmap
    
    optionText =
        "\npasswdmap options: \n\
         \\n\
         \1   Look up a user name\n\
         \2   Look up a UID\n\
         \3   Display entire file\n\
         \4   Quit\n\n\
         \Your selection: "
        
    