{-# OPTIONS_GHC -Wall #-}
-- Chapter and Exercises from
-- http://book.realworldhaskell.org/read/io.html
-- Repeating the questions part in this file, for easy read.

import System.IO
import Data.Char(toUpper)

import System.Directory(getTemporaryDirectory, removeFile, renameFile)
import System.IO.Error(catchIOError)
import Control.Exception(finally)

-------------------------------------------------------------------------------
-- Chapter Walkthrough
-------------------------------------------------------------------------------

basicio :: IO()
basicio = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
-- Try :t putStrLn and :t getLine in ghci
-- getLine :: IO String  (getLine is storing an IO action!)
-- "<-" binds the result to inpStr, so inpStr is of type "String"
-- The <- operator is used to "pull out" the result from performing an 
-- I/O action and store it in a variable.
-- main (here basicio) is of type IO()
    
-- all haskell programs start with main and execute all IO within that.
-- "do" is convenient to sequence actions
-- "do" needed when more than one action needs to be performed.
-- indentation is important within "do" block.

----------------
{-
ghci> let writefoo = putStrLn "foo"
ghci> writefoo
foo
    
The output foo is not a return value from putStrLn. It is the side effect of 
putStrLn actually writing foo to the terminal
-}

-- IO Action
-- type IO t
-- Produce an effect when performed, but not when evaluated
-- Performing (executing) an action of type IO t may perform I/O and will 
-- ultimately deliver a result of type t

name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

callingpure :: IO ()
callingpure = do
       putStrLn "Greetings once again.  What is your name?"
       inpStr <- getLine
       let outStr = name2reply inpStr       -- (note: No "let .. in" only "let")
       putStrLn outStr
-- "<-" pull the value from the IO (or any monad) context
-- "let .. =" assigns the value (in a non IO or non Monad context)

----------------
-- Code Snippets to do file operation
toupperImp :: FilePath -> FilePath -> IO ()
toupperImp inF outF = do 
       inh <- openFile inF ReadMode
       outh <- openFile outF WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()   -- not the return in C - this put () in IO, so IO()
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map toUpper inpStr)
                   mainloop inh outh       
                   
-- return puts a pure value () in monad context () becomes IO ()
-- return is opposite of "<-", <- is used to extract the pure value from context


-- We opened the files with ReadMode and WriteMode
-- Deatils on IO modes:
-- ReadMode - Can read, Cannot write, Seek Pos: Start. File should present
-- WriteMode - Cannot read, Can write, Seek Pos: Start. File Emptied when exists
-- ReadWriteMode - Can read, Can write, Seek Pos: Start. Created if not exists,
--                 left untouched if existing already
-- AppendMode - Cannot read, Can Write, Seek Pos: End. Created if not exists, 
--              left untouched if existing already



-- New functions:
-- openFile, hClose, hIsEOF, hGetLine, hPutStrLn
-- use openBinaryFile to process binary files
-- Linux treats both text and binary files the same way. Need to use
-- openBinaryFile function to have compatibility with Windows OS

----------------

-- hTell: To tell where the read or write position is in the file.
-- hSeek: To go to desired location in the file.
--        Takes 3 arguments
--        hSeek handle SeekFromEnd 0
--        Handle, SeekMode and SeekValue
--        SeekMode can be one of SeekFromEnd, AbsoluteSeek or RelativeSeek
--        (from end, from start, from current respectively)

-- hIsSeekable: Tells whether a handle is seekable (ex: n/w handle is not)

-- Standard Input, Output and Error
-- for every h-function there is a non-h function.
-- non-h function are shortcuts for h-functions with std input / output 

{- In the partial function application terms, 
getLine = hGetLine stdin
putStrLn = hPutStrLn stdout
print = hPrint stdout 
-}

{-
Sometimes better to use standard input and output instead of files.
Works with terminal. Can combine with other programs.

$ echo John | runghc callingpure.hs
Greetings once again.  What is your name?
Pleased to meet you, John.
Your name contains 4 characters.

-}

-- file: ch07/tempfile.hs
-- Explains various concepts including
-- System.Directory module
-- fSeek, fTell
-- renameFile, removeFile
-- openTempFile, openBinaryTempFile 
--    (needs 1. directory to create the temp file, 2. template for naming)
-- Use "." for current as temporary directory 
-- Use "System.Directory.getTemporaryDirectory" function to get the best tempdir
-- return of openTempFile :: (FilePath, Handle)

-- The main entry point.  Work with a temp file in myAction.
tempFileMain :: IO ()
tempFileMain = withTempFile "mytemp.txt" myAction

{- The gist of the program.  Called with the path and handle of a temporary
   file.  When this function exits, that file will be closed and deleted
   because myAction was called from withTempFile. -}
myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
    do -- Start by displaying a greeting on the terminal
       putStrLn "Welcome to Temporary File Operations"
       putStrLn $ "I have a temporary file at " ++ tempname

       -- Let's see what the initial position is
       pos <- hTell temph
       putStrLn $ "My initial position is " ++ show pos

       -- Now, write some data to the temporary file
       let tempdata = show [1..10]
       putStrLn $ "Writing one line containing " ++ 
                  show (length tempdata) ++ " bytes: " ++
                  tempdata
       hPutStrLn temph tempdata

       -- Get our new position.  This doesn't actually modify pos
       -- in memory, but makes the name "pos" correspond to a different 
       -- value for the remainder of the "do" block.
       newPos <- hTell temph
       putStrLn $ "After writing, my new position is " ++ show newPos

       -- Seek to the beginning of the file and display it
       putStrLn $ "The file content is: "
       hSeek temph AbsoluteSeek 0

       -- hGetContents performs a lazy read of the entire file
       c <- hGetContents temph

       -- Copy the file byte-for-byte to stdout, followed by \n
       putStrLn c

       -- Let's also display it as a Haskell literal
       putStrLn $ "Which could be expressed as this Haskell literal:"
       print c

{- This function takes two parameters: a filename pattern and another
   function.  It will create a temporary file, and pass the name and Handle
   of that file to the given function.

   The temporary file is created with openTempFile.  The directory is the one
   indicated by getTemporaryDirectory, or, if the system has no notion of
   a temporary directory, "." is used.  The given pattern is passed to
   openTempFile.

   After the given function terminates, even if it terminates due to an
   exception, the Handle is closed and the file is deleted. -}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do -- The library ref says that getTemporaryDirectory may raise on
       -- exception on systems that have no notion of a temporary directory.
       -- So, we run getTemporaryDirectory under catch.  catch takes
       -- two functions: one to run, and a different one to run if the
       -- first raised an exception.  If getTemporaryDirectory raised an
       -- exception, just use "." (the current working directory).
       tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
       (tempfile, temph) <- openTempFile tempdir pattern 

       -- Call (func tempfile temph) to perform the action on the temporary
       -- file.  finally takes two actions.  The first is the action to run.
       -- The second is an action to run after the first, regardless of
       -- whether the first action raised an exception.  This way, we ensure
       -- the temporary file is always deleted.  The return value from finally
       -- is the first action's return value.
       finally (func tempfile temph) 
               (do hClose temph
                   removeFile tempfile)
                   
------------------------
-- To Upper (Lazy Implementation)

toupperLazy :: IO ()
toupperLazy = do 
    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    inpStr <- hGetContents inh          -- close handle only after consuming.
    hPutStr outh (processData inpStr)
    hClose inh
    hClose outh

-- once the processData is done, the inpStr memory is freed
-- inpStr is what read using hGetContents (could be 20 bytes or 500 GB!)
processData :: String -> String
processData = map toUpper
    
------------------------
-- To Upper with readFile and writeFile functions
-- readFile is the combo of openFile and hGetContents
-- readFile :: FilePath -> IO String
-- writeFile is the combo of openFile WriteMode and writing contents
-- writeFile :: FilePath -> String -> IO ()

toupperLazy2 :: IO ()
toupperLazy2 = do 
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)
-- no handles associated with readFile and writeFile
-- no handles to close here!

{-
Will the call to putStr or writeFile force the entire input string to be loaded 
into memory at once, just to be written out? 
The answer is no. 

putStr (and all the similar output functions) write out data as it becomes 
available. They also have no need for keeping around data already written, 
so as long as nothing else in the program needs it, the memory can be freed 
immediately. The string between readFile and writeFile is kind of a pipe 
linking the two. Data goes in one end, is transformed some way, 
and flows back out the other. 

Larger files take some time to process, but there is a constant—and low—memory 
usage while it is being processed. (so, the entire file is not loaded at once)

-}

------------------------
-- interact function (Read from standard input and write out to stdout)
-- takes a function of type (String -> String)
-- does "getContents stdin" and the result is sent to stdout
makeUpper :: IO ()
makeUpper = interact (map toUpper)
-- But, can work with other files too, with piping in runghc
-- runghc toupper-lazy4.hs < input.txt > output.txt

-- this will work with piping the input, not in the ghci. see makeUpper.hs
makeUpper2 :: IO ()
makeUpper2 = interact ((++) "Your data, in uppercase, is:\n\n" . 
                 map toUpper)
                 