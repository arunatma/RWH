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
import Data.Int (Int64)
import Data.Word (Word8)

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

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise = Nothing    
    
-- "nat" is natural number (to get width, height and max)
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
                Nothing -> Nothing 
                Just (num, rest)
                    | num <= 0  -> Nothing
                    | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count              = fromIntegral n
                     both@(prefix, _)   = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both
    
-- parsing function
-- parse - takes the input ByteString and parses it and returns a tuple that 
-- contains the parsed Greymap along with the remaining string
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
    case matchHeader (L8.pack "P5") s of
        Nothing -> Nothing
        Just s1 -> 
            case getNat s1 of
                Nothing -> Nothing
                Just (width, s2) -> 
                    case getNat (L8.dropWhile isSpace s2) of
                        Nothing -> Nothing
                        Just (height, s3) ->
                            case getNat (L8.dropWhile isSpace s3) of
                                Nothing -> Nothing
                                Just (maxGrey, s4)
                                    | maxGrey > 255 -> Nothing
                                    | otherwise ->
                                        case getBytes 1 s4 of
                                            Nothing -> Nothing
                                            Just (_, s5) ->
                                                case getBytes (width * height) s5 of
                                                    Nothing -> Nothing
                                                    Just (bitmap, s6) ->
                                                        Just (Greymap width height maxGrey bitmap, s6)
                                                        

-- Reducing the boiler plate (repetitive code)
-- Two patterns in the above set of functions
-- 1st pattern: 2nd argument is L.ByteString and return type is Maybe value
-- 2nd pattern: Every step in parseP5 fn deconstructs a Maybe value 

-- Capturing the second pattern using a function
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _  = Nothing
Just v  >>? f  = f v

-- No fixity declaration for (>>?)
-- So, default setting to infixl 9 
-- i.e (left associative, strongest operator precedence)
-- a >>? b >>? c === ((a >>? b) >>? c)

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s      >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) -> getNat s           >>?
    skipSpace                         >>?
    \(height, s) -> getNat s          >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)
    
skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)    

-- parseP5_take2 is done using chaining >>?
-- On the left hand side of each (>>?) is a Maybe value; 
-- On the right is a function that returns a Maybe value.

-- Implicit State
-- The current code will not be able to tell the exact location where the parse
-- has failed.  If we want to implement that we would need a 3-tuple to be 
-- passed around - Needs entire re-writing of code and a messy pass around.

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)
    
    
-- with this change to Algebraic Data Type; both residual string and the offset 

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

-- to help with error
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

-- Future proof the code: Do not expose the implementation details
-- Wrap it with "newtype" wrapper 

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}
-- usage of record syntax here
-- runParse is the accessor function auotmatically derived
-- runParse :: Parse a -> (ParseState -> Either String (a, ParseState)
-- runParse gets a Parse data type and returns a function that can take a 
-- ParseState and returns Either String (a, ParseState)

-- identity parser (gives whatever is input as the return of the parser)
identity :: a -> Parse a
identity a = Parse (\s -> Right(a, s))

-- "Parse" value constructor takes in a function - that function takes in a 
-- ParseState and returns a Either data type
-- Here, the identity parser creates a 'Parse' to which any state is passed
-- returns with the same state 's'.  The argument given as input is used in 
-- the result 'a'

-- Using the wrapped function (ParseState -> Either (a, ParseState)
-- 'parse' function
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of 
        Left err            -> Left err
        Right (result, _)   -> Right result
        
-- 'parse' or 'identity' functions do not even inspect what the state 's' is!

-- Using the record syntax for modifying offset
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState {offset = newOffset}

{-
    ghci> let before = ParseState (L8.pack "foo") 0
    ghci> let after = modifyOffset before 3
    ghci> before
    ParseState {string = Chunk "foo" Empty, offset = 0}
    ghci> after
    ParseState {string = Chunk "foo" Empty, offset = 3}
-}        

-- Parser to parse a single byte

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L8.uncons (string initState) of
        Nothing                 -> bail "no more input"
        Just (byte, remainder)  -> 
            putState newState ==> \_ -> identity byte
            where
                newState  = initState {string = remainder, offset = newOffset}
                newOffset = offset initState + 1
                
{-
    L8.uncons takes the first element from the ByteString and presents the 
    remainder
    
    ghci> L8.uncons (L8.pack "foo")
    Just ('f',Chunk "oo" Empty)
    ghci> L8.uncons L8.empty
    Nothing
    
    getState function retrieves the current parsing state
    putState function replaces the parsing state
    bail     function terminates and reports an error
-}                

-- getState and putState functions
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

-- The left element of the tuple is the result of a Parse (ref. by a usually), 
-- The right is the current ParseState (s). 

-- getState fn extracts the current parsing state so we can access to the
-- string and the offset
-- putState fn replaces the current ParseState with a new one taken as input

-- Need not pass tuples around now
-- Can get done with the use of getState and putState functions

-- Details of the parsing state now in ParseState
-- Instead of pattern matching, we now use the accessor functions 
-- Now if new info is needed in the ParseState, we can change the data type,
-- add new info with its accessor and only those new functions that need the 
-- new info will use the new accessor. No touching the existing fns which are
-- using the other info from ParseState.
-- If we had used pattern matching, all code needs to be retouched to handle 
-- the new info in ParseState

-- Two more functions to be defined which are in parseByte function
-- one is "bail" and another is "(==>)"

bail :: String -> Parse a
bail err Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

-- (==>) chains the parsers together
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState = 
        case runParse firstParser initState of
            Left errMessage               -> Left errMessage
            Right (firstResult, newState) -> 
                runParse (secondParser firstResult) newState
                


