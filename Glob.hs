-- Glob.hs
-- part of chapter 8: Efficient file handling and regular expressions
-- http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html

module Glob (namesMatching) where

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
                         
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

import Control.Exception (handle, SomeException)
import Control.Monad (forM)
import GlobRegex (matchesGlob)

{-
    ghci> dropTrailingPathSeparator "foo/"          -- "foo"
    
    Separate the string with the last file separator
    ghci> splitFileName "foo/bar/Quux.hs"           -- ("foo/bar/", "Quux.hs")
    
    join file paths
    ghci> "foo" </> "bar"                           -- "foo/bar" in unix
                                                    -- "foo\\bar" in windows
    
-}

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

-- doesNameExist is a custom fn for file and dir (defined below)
namesMatching ignoreCase pat
    | not (isPattern pat) = do
        exists <- doesNameExist pat     
        return (if exists then [pat] else [])
    -- if it is a glob pattern:    
    | otherwise = do
        case splitFileName pat of 
            ("", baseName) -> do
                curDir <- getCurrentDirectory
                listMatches ignoreCase curDir baseName
            (dirName, baseName) -> do
                dirs <- if isPattern dirName
                        then namesMatching ignoreCase (dropTrailingPathSeparator dirName)
                        else return [dirName]
                let listDir = if isPattern baseName
                              then listMatches
                              else listPlain
                pathNames <- forM dirs $ \dir -> do
                                baseNames <- listDir ignoreCase dir baseName
                                return (map (dir </>) baseNames)
                return (concat pathNames)
                
-- watchout for splitFileName
-- splitFileName "foo/" will return ("foo/", "")  
-- make sure to use dropTrailingPathSeparator function


-- forM function acts a little like a “for” loop: 
-- Maps the second argument (an action) over the first (a list)
-- Returns the list of results

-- return true if you find a dir or file in the given name.
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
        then return True
        else doesDirectoryExist name
        
-- listMatches returns the list of files matching the given glob pattern.
listMatches :: Bool -> FilePath -> String -> IO [String]
listMatches ignoreCase dirName pat = do
    handle handler action
    where 
        handler :: SomeException -> IO [String]
        handler = const (return [])
        action = do 
            dirName' <- directory dirName
            names <- getDirectoryContents dirName'
            let names' = if isHidden pat
                         then filter isHidden names
                         else filter (not . isHidden) names
            return (filter (\name -> matchesGlob name pat ignoreCase) names')
            
directory :: String -> IO FilePath
directory dirName = if null dirName
                    then getCurrentDirectory
                    else return dirName

isHidden ('.':_) = True
isHidden _       = False

{-
handle :: (Exception -> IO a) -> IO a -> IO a

handle defined in Control.Exception
handle takes two arguments. 
    1. function (Exception -> IO a)
    2. IO a  (the actual IO action to run)
    
    If an exception is thrown running (2) above then IO a in (1) is run.
    
const ::  a -> b -> a  (takes two arguments, ignore the first; return the 2nd)
Use 'const' function to write an exception handler that 
ignores the exception it is passed
-}

-- listPlain return an empty or singleton list (list with a single item)
listPlain :: Bool -> FilePath -> String -> IO [String]
listPlain ignoreCase dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])

-- Exercise
-- 1. Make case ignoring feature without altering the signature of namesMatching
-- 2. Replacement fn (from System.Posix.Files) for doesNameExist in Unix systems
-- 3. Implement matching on "**" wild cards, to recursively search on sub
--    directories.

