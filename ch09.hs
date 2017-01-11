{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad (forM, filterM)
import System.Directory (Permissions(..), getPermissions, getModificationTime, 
    doesDirectoryExist, getDirectoryContents)
import System.Time (ClockTime(..))    
import System.FilePath ((</>), takeExtension)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Data.Time

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


-- As of now the predicate can look at only file names
-- How to get files greater than or less than specific size?
-- Remember: Better to have the predicate type as FilePath -> Bool instead 
--    of FilePath -> IO Bool to save us from unwanted side effects.

{-

    ghci> getPermissions "."
        Permissions {readable = True, writable = True, 
        executable = False, searchable = True}
        
    ghci> getModificationTime  "."
        2017-01-10 04:29:29.1053908 UTC
        
    getModificationTime
        :: FilePath -> IO time-1.5.0.1:Data.Time.Clock.UTC.UTCTime
    getModificationTime
        :: FilePath -> IO System.Time.ClockTime
-}

-- Making a better predicate
type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime       -- last modified
               -> Bool
-- So, Predicate is a type synonym for a fn of 4 arguments returning Bool.

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check 
    where check name = do       -- check :: FilePath -> IO Bool
            perms <- getPermissions name
            size <- return Nothing --getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

{-
    Monad Filter - Take a predicate returning Monadic Bool and a list, returns 
    the filtered list encapsulated in the monad.
    
    ghci> :type filterM
    filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
-}

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

-- return a Maybe type if the type is not a plain file.
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle handler action
    where handler = (\(_ :: SomeException)  -> return Nothing)
          action = do
            h <- openFile path ReadMode
            size <- hFileSize h
            hClose h
            return (Just size)

-- There are directory entries on which openFile will succeed, but hFileSize 
-- will throw an exception. (Ex: for named pipes). That will be caught by 
-- handle, and call to hClose will never occur!
-- handles are precious resource, we cannot wait till the garbage collector 
-- auto closes the open handles.

-- use 'bracket' instead of 'handle' from Control.Exception

{-
    ghci> :type handle
    handle :: GHC.Exception.Exception e => (e -> IO a) -> IO a -> IO a
   
    "handle handler action"
    
    ghci> :type bracket
    bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    
    "bracket acquirer releaser user"        (of a resource)
    -- an exception occurs while "user" performing, then "releaser" called.
    -- the exception is thrown up after executing "releaser"
-}

getFileSize :: FilePath -> IO (Maybe Integer)       
getFileSize path = handle (\_ -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
