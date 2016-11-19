-- Real World Haskell
-- Chapter 8: Efficient file processing, regular expressions, and 
-- file name matching
-- http://goo.gl/8ru8dU
-- imports FileProcCh08.hs (but independent on its own)


-- getContents takes from standard input; or file contents can be piped into it	
-- create an executable file out of this .hs file
-- Compile into an "exe" 
    -- > ghc --make ExecCh08.hs  

-- Execution:
-- 1. (to give input in command line)
-- > ExecCh08.exe
-- or
-- > runhaskell ExecCh08.hs 

-- 2. (to take input from a text file)
-- > type sumFileIn.txt | ExecCh08.exe
-- > or
-- > type sumFileIn.txt | runhaskell ExecCh08.hs


import FileProcCh08 (sumNumsInFile)    

main = sumNumsInFile