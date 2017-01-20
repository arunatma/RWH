-- Real World Haskell
-- Chapter 10: Code case study: parsing a binary data format
-- http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html

-- Objectives of the chapter
-- 1. Parsing
-- 2. Program Organization

-- Parsing the files from netpbm suite 
-- https://en.wikipedia.org/wiki/Netpbm
-- Filetypes:
-- 1. Portably aNy Map (PNM)
--      Portable Bit / Grey / PixMap formats (PBM, PGM, PPM)
-- 2. Portable Arbitrary Map (PAM)

-- Our focus here is PGM (and in that only Raw PGM)
-- Files can be of any of the two formats in PGM 1. Plain 2. Raw
-- Each has header (magic string + props) followed by image data
-- Plain can have only one image per file. Raw can have many images each with 
-- its own header followed by image data.
-- The Header:
--      Magic String (P2 means plain and P5 means raw)
--      White Space
--      Width in ASCII followed by a White Space
--      Height in ASCII followed by a White Space
--      Maximum Grey Value in ASCII followed by a White Space
-- 1. Plain Content
--      Image Data (in ASCII numbers) each separated by a white space
-- 2. Raw Content
--      Image Data (string of binary values)

-- Header is ASCII text and Body is binary
-- We need both text and binary oriented bytestrings
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)
    
-- greyData may contain huge content. default 'show' makes to display a string 
-- which is unusually huge in size. So, do not derive Show typeclass
-- Similarly no point in deriving Read - or writing a Read instance

-- Writing our own instance of Show for Greymap
instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m

-- parsing function
-- parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
