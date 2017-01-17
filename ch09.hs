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

import Control.Monad (forM, filterM, liftM)
import System.Directory (Permissions(..), getPermissions, getModificationTime, 
    doesDirectoryExist, getDirectoryContents)
import System.Time (ClockTime(..))    
import System.FilePath ((</>), takeExtension, takeFileName)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Data.Time
import Data.Char (toLower)

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
-- remember that this is a fn, argument is directory name. 

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
            size <- getFileSize name
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
getFileSize path = handle handler action
    where 
        handler :: SomeException -> IO (Maybe Integer)
        handler = (\_ -> return Nothing)
        action = bracket (openFile path ReadMode) hClose $ \h -> do
            size <- hFileSize h
            return (Just size)

-- A domain specific language for predicates
-- Writing a predicate to search for a C++ file > 128KB
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 128 * 1024
myTest _ _ _ _              = False

-- making the predicate function better
type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> UTCTime       -- last modified
             -> a

-- defining separate predicates for path and size
-- InfoP FilePath is a type synonym for the function with type signature
-- (FilePath -> Permissions -> Maybe Integer -> UTCTime -> FilePath)
-- So, pathP is a function taking four args. and returning a String (FilePath)
pathP :: InfoP FilePath
pathP path _ _ _ = path

-- InfoP Integer is a type synonym for the function with type signature
-- (FilePath -> Permissions -> Maybe Integer -> UTCTime -> Integer)
-- So, sizeP is a function that takes four arguments and returns an Integer
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

-- check whether the predicate is of a given value 'k'
-- take an InfoP function, and the given value 'k', return a function that 
-- behaves as below:
-- The return function takes four arguments, apply the InfoP fn over the 4 
-- arguments and checks the value of fn is equal to the value 'k' supplied.
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

-- "InfoP Bool" is nothing but a synonym for "Predicate"
-- So, the return type of equalP function is a "Predicate"

-- same equalP fn, using currying instead of lambda function.
equalP' :: (Eq a) => InfoP a -> a -> Predicate
equalP' f k w x y z = f w x y z == k


-- checking whether sizeP works and type checks!
{-
    ghci> :type betterFind (sizeP `equalP` 1024)
    betterFind (sizeP `equalP` 1024) :: FilePath -> IO [FilePath]
-}

-- equalP checks for (==)
-- we need to write similar functions for other binary operations (>), (<) etc
-- That would mean boiler plating.
-- But, not needed in Haskell

-- make the fn operator as an argument, instead of writing one fn for each
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k     -- look at the similarity with equalP

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

-- lifting lets us reduce the boiler plating and reusing the code.
-- also look at the intentional placement of 'q' in liftP
-- That helped in writing greaterP and lesserP concise!

-- Gluing Predicates together
-- Plain simple way!
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

-- Using the lifting concept
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

-- writing liftP in terms of liftP2
constP :: a -> InfoP a
constP k _ _ _ _ = k

-- liftP' q f k w x y z = f w x y z `q` constP k w x y z
-- So, 
liftP' q f k = liftP2 q f (constP k)

-- Combinators!
-- The functions taking other functions as arguments and returning functions.
-- lift category of functions are all combinators.

-- Rewriting the myTest function using our lift functions (defined above)
{-
    myTest path _ (Just size) _ =
        takeExtension path == ".cpp" && size > 131072
    myTest _ _ _ _ = False    
-}

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` (128 * 1024))

-- Defining new infix operators
(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP
(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?)  = greaterP
(<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(<?)  = lesserP

(&&?) = andP
(||?) = orP

-- rewriting the myTest2
myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

-- in the expression above, the parentheses are necessary
-- otherwise the the expression is evaluated from left to right
-- (((liftPath takeExtension) ==? ".cpp") &&? sizeP) >? 131072,  NOT OUR NEED!!
-- treated as inflixl 9
-- To overcome that, we need to write fixity declarations

-- Check the fixity of the normal operators like (==) and (&&)
{-
    ghci> :info ==
    class Eq a where
      (==) :: a -> a -> Bool
      ...
        -- Defined in GHC.Base
    infix 4 ==
    ghci> :info &&
    (&&) :: Bool -> Bool -> Bool 	-- Defined in GHC.Base
    infixr 3 &&
    ghci> :info >
    class (Eq a) => Ord a where
      ...
      (>) :: a -> a -> Bool
      ...
        -- Defined in GHC.Base
    infix 4 >
-}
-- So, == and > are infix 4,  && is infixr 3

-- Defining these for our lift operators
infix 4 ==?
infixr 3 &&?
infix 4 >?

-- now, rewriting myTest3 without paranthesis
myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

-- Controlling the traversal
-- When traversing the filesystem, we'd like to give ourselves more control 
-- over which directories we enter, and when. An easy way in which we can 
-- allow this is to pass in a function that takes a list of subdirectories of a 
-- given directory, and returns another list. 
-- This list can have elements removed, or it can be ordered differently than 
-- the original list, or both. 

-- The simplest such control function is 'id', which will return its input 
-- list unmodified

-- We need to restrict the traversal and the order of traversals.  We need 
-- some other data structure (similar one) specialized for the same.
-- Instead of InfoP a, defining an algebraic data type "InfoP"
data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)
-- as we used the record syntax, we got 4 accessor functions like "infoPath",
-- "infoPerms", without explicitly defining them
 
traverse' :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse' order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse' order (infoPath info)
            else return [info]
-- liftM above applies concat function to the strings inside the IO monad.
-- So, liftM lifts the concat fn (normally applid to [[String]]) so as it can 
-- be applied to IO [[String]]
            
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)
    
isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle handler action 
    where handler :: SomeException -> IO (Maybe a)
          handler = (\_ -> return Nothing)
          action  = (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)
    
-- Exercises
-- 1. What should you pass to traverse to traverse a directory tree in reverse
--    alphabetical order?     ('reverse')
-- 2. Using 'id' as a control function "traverse id" performs preorder 
--    traversal.  Write a control fn that performs post-order traversal.
--    Preorder traversal means parents directory before listing the children
-- 3. Make the predicates and combinators to work for the new "Info" type
-- 4. Fn wrapper for traverse with two predicates - one for traversal and 
--    and another for filtering the results

-- Same "traverse" function which is less dense and more verbose.
traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
                                Nothing -> False
                                Just perms -> searchable perms
        recurse info = do
            if isDirectory info && infoPath info /= path
            then traverseVerbose order (infoPath info)
            else return [info]
            
-- How this is different?
-- Local definitions instead of using partial applications
-- case expression for 'isDirectory' instead of 'maybe' combinator
-- No use of liftM here - replaced by manual 'concat' and 'return'
-- local functions isDirectory and getUsefulContents merged in the fn itself.

-- Core idea: Find the balance between density (conciseness) and readability!
-- See the story on Andrew, Zack and Monica here - interesting. Read comments!


-- Short fall still existing in traverse function:
-- To filter the list, still we need all names in the list for 'order' function 
-- [Info] -> [Info] to work.
-- Possible Soulution:
-- Think of file system traversal as a 'fold' over the directory hierarchy!

-- foldr and foldl'
-- also add an element of control to the fold (represented by Algebraic Datatype)
data Iterate seed = Done     {unwrap :: seed}
                  | Skip     {unwrap :: seed}
                  | Continue {unwrap :: seed}
                    deriving (Show)
                    
type Iterator seed = seed -> Info -> Iterate seed  -- this is the fold fn.
-- Take a seed and a folder structure (represented by Info) and returns a new 
-- seed along with the instruction (to skip, continue or done already)
-- The instructions are the constructors of the Iterate data type

-- If the instruction is 'Done' : traversal stops immediately
-- If the instruction is 'Skip' : Traversal will not recurse into the directory
-- If the instruction is 'Continue' : Continue, using the wrapped value as 
--   input to the next fold function.

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
        endSeed <- fold initSeed path
        return (unwrap endSeed)
    where
        fold seed subpath = getUsefulContents subpath >>= walk seed
        
        walk seed (name : names) = do
            let path' = path </> name
            info <- getInfo path'
            case iter seed info of
                done@(Done _ ) -> return done
                Skip seed'     -> walk seed' names
                Continue seed'
                    | isDirectory info -> do
                        next <- fold seed' path'
                        case next of
                            done@(Done _) -> return done
                            seed''        -> walk (unwrap seed'') names
                    | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)

-- foldTree fn is just a wrapper for 'fold' function 
-- fold is a local function: so no need to pass the 'iter' argument
-- walk fn can get path from foldTree's argument.
-- walk is a tail recursive loop (adv: can stop early, if need be)
-- fold calls walk; walk calls fold to traverse to sub directories.

-- Iterator in practice: (an example input to foldTree)
-- Looks into at most 3 bmp images; and not traversing to subversion folders
atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
    | length paths == 3
      = Done paths
    | isDirectory info && takeFileName path == ".svn"
      = Skip paths
    | extension `elem` [".jpg", ".png"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info
        
-- To use this:
-- foldTree atMostThreePictures []

-- another iterator
countDirectories count info =
    Continue (if isDirectory info
              then count + 1
              else count)        

-- totalDirs fn takes a path and counts the total subdirs in the path.
totalDirs = foldTree countDirectories 0
