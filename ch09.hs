-- Real World Haskell
-- Chapter 9: I/O case study: a library for searching the filesystem
-- http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html

-- "I know I have this file, but I don't know where it is"
-- The first 'find' command introduced in unix systems in 1974!
-- find with 
--      1. names matching this glob pattern
--      2. entry is a plain file
--      3. last modified before this date
--      4. More such and with combinations of two or more using "and" "or"

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension)

-- Recursively listing a directory
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let filesAndDirs = filter (`notElem` [".", ".."]) names
    paths <- forM filesAndDirs $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path  -- IO Bool changed to Bool 
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
    
-- forM and mapM takes functions as arguments
-- forM takes a function (here it is lambda fn) which contains the loop code.
-- make the function generic and define outside, if needed to use by other 
-- functions.
{-
    ghci> :type mapM
    mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
    ghci> :type forM
    forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
    
    Note:  mapM = flip forM
    
    Why 2 functions?
    If we were to use mapM instead of forM, we'd have to place the variable 
    properNames after the body of the function! Readability goes for a toss!
    
    If the body of the loop is a named function, and the list over which we 
    loop is computed by a complicated expression, mapM will serve better.
-}

-- Native finding function (using getRecursiveContents)
simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)

-- testing the fn 
-- find all files with ".c" extension recursively    
filesWithExtC = simpleFind (\p -> takeExtension p == ".c")
-- this is a function that needs to be supplied the directory name 

filesWithExtHS = simpleFind (\p -> takeExtension p == ".hs")

{-
    Problems with simpleFind
    1. Directories ending  with ".c" will also be listed
    2. No control over how the filesystem is traversed (traverses through
       unncessary git and svn folders)
    3. Strict evaluation because of usage of IO monad. Poor resource usage and 
       responsiveness
    4. Predicate is cumbersome looking
-}


