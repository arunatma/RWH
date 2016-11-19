-- Real World Haskell
-- Chapter 8: Efficient file processing, regular expressions, and 
-- file name matching
-- http://goo.gl/8ru8dU

module FileProcCh08 where

import qualified Data.ByteString.Lazy as L

-- importing for Text I/O
import qualified Data.ByteString.Lazy.Char8 as LT

-- Reads a text file full of numbers and print the sum
sumNumsInFile = do
    contents <- getContents
    print (sumFile contents)
    where sumFile = sum . map read . words
    
-- String is expensive; So, use bytestrings
-- Code written using String cannot even match Python in performance
-- Code written using bytestring may sometimes be equivalent to C in performance

-- Example using bytestring
-- Function to determine whether a file is an ELF object file (exe on unix)
-- A byte seq. that identifies a file type is called a 'magic number'

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]
    
isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElfMagic content)    
    
-- lazy readFile does an efficient read of  64KB at a time!


--------------------------------------------------------------------------------

-- Finding the highest closing price from a csv file
-- refer prices.csv for the input format

closing = readPrice . (!!4) . LT.split ','

readPrice :: LT.ByteString -> Maybe Int
readPrice str =
    case LT.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case LT.readInt (LT.tail rest) of
          Nothing           -> Nothing
          Just (cents,more) ->
            Just (dollars * 100 + cents)
            
highestClose = maximum . (Nothing:) . map closing . LT.lines

highestCloseFrom path = do
    contents <- LT.readFile path
    print (highestClose contents)
--------------------------------------------------------------------------------    
    
