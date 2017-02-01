{-# LANGUAGE FlexibleInstances #-}
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
import Data.Char (isSpace, chr, isDigit)
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
    case L.uncons (string initState) of
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
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

-- (==>) chains the parsers together

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState = 
            case runParse firstParser initState of
                Left errMessage               -> 
                    Left errMessage
                Right (firstResult, newState) -> 
                    runParse (secondParser firstResult) newState
                

-- Parse type is a wrapper to a function
-- (==>) combines two such parse functions - Parse, take out the value, pass on
-- the value derived from first to second parser.

-- Functors
-- Revisiting map function
plus1 = map (+1) [1, 2, 3]
showMe = map show [1, 2, 3]

-- map like function in binary tree
data Tree a = Node (Tree a) (Tree a) 
            | Leaf a
            deriving (Show)
            
-- let us assume the tree contain strings (Tree String)
-- to get the length of strings in each of the node (again in the form of Tree)
treeLengths :: Foldable t => Tree (t a) -> Tree Int
treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

-- what if instead of finding the lengths, we need to prefix 'a' to each?
-- Abstraction helps
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

-- now treeLengths fn in terms of treeMap is:
treeLen :: Tree [Char] -> Tree Int
treeLen = treeMap length

-- to prefix with 'a'
treePrefix :: Tree [Char] -> Tree [Char]
treePrefix = treeMap ('a':)
{-
    ghci> let tree = Node (Leaf "foo") (Node (Leaf "x") (Leaf "quux"))
    ghci> treeLen tree
    Node (Leaf 3) (Node (Leaf 1) (Leaf 4))

    ghci> treePrefix tree
    Node (Leaf "afoo") (Node (Leaf "ax") (Leaf "aquux"))
    
    ghci> treeMap (even . length) (treePrefix tree)
    Node (Leaf True) (Node (Leaf True) (Leaf False))
    
-}

-- Haskell has a type-class to generalise treeMap
{-
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
-}    

-- Any data type to want to be an instance of the Functor type-class should
-- implement the 'fmap' function.  

-- fmap is a lifting function.  Lifts the function from the plain context to 
-- that of the container context
-- fmap :: (a -> b) -> (f a -> f b)
-- another way to look at fmap:  Take a function that operates on plain values
-- Give me another function that does the same on contextual values

-- making Tree an instance of a Functor
instance Functor Tree where
    fmap = treeMap

-- making list [] an instance of a Functor (already done in GHC.Base)
{-    
    instance Functor [] where
        fmap = map    
-}    

-- making Maybe an instance of a Functor (already done in GHC.Base)
{-    
    instance Functor Maybe where
        fmap _ Nothing = Nothing
        fmap f (Just x) = Just (f x)
-}    

-- We can make Functor instances for datatypes with only one parameter
-- We cannot do it for Either a b or (a, b)

-- Also no constraint on data type definition

-- Valid Functor
data Foo a = Foo a
           
instance Functor Foo where
    fmap f (Foo a) = Foo (f a)
    

-- Invalid to write a Functor instance on Bar, as it has a constraint
{-
    data Eq a => Bar a = Bar a
    
    instance Functor Bar where
        fmap f (Bar a) = Bar (f a)
-}
-- Better have the constraints on the function definitions

-- Adding a constraint to a type definition is never a good idea. 
-- That forces you to add type constraints to each function that operates on 
-- the values of that type. This is deprecated in the latest versions
{- 
    data (Ord a) => OrdStack a = Bottom
                               | Item a (OrdStack a)
                                 deriving (Show)
-}

-- infix use of fmap
fmapEx1 = (1+) `fmap` [2,3,4] ++ [5,6,7]    -- [3,4,5,5,6,7]
fmapEx2 = (1+) `fmap` ([2,3,4] ++ [5,6,7])  -- [3,4,5,6,7,8]
fmapEx3 = (1+) <$> ([2,3,4] ++ [5,6,7])     -- [3,4,5,6,7,8]
-- (<$>) is nothing but fmap infix operator defined in Data.Functor
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

--Flexible Instances
--------------------
-- We cannot create a functor for a data type with more than one variable, but
-- we can do so it we make all others concrete expcept for one
-- Not possible to create Functer for Either 
-- But, it is possible to create functor instance for Either a

{-
Already defined in GHC.Base.
To define this we need to use the Language Pragma Flexible Instances

    instance Functor (Either a) where
        fmap g (Right x) = Right (g x)
        fmap g (Left x) = Left x    
-}

-- Functor Laws
lhsFunctorLaw1 = id (Node (Leaf "a") (Leaf "b"))
rhsFunctorLaw1 = fmap id (Node (Leaf "a") (Leaf "b"))
--  Law 1: fmap id == id 

lhsFunctorLaw2 = (fmap even . fmap length) (Just "twelve")
rhsFunctorLaw2 = fmap (even . length) (Just "twelve")
--  Law 2: fmap (f . g) == fmap f . fmap g

-- Functor instance for Parse data type
-- The function that we are 'fmap'ping should be applied to the current result
-- of the parse, with the parse state untouched. So, use the 'identity' function
-- and map the function 'f' only on the result.

instance Functor Parse where
    fmap f parser = parser ==> \result -> 
                    identity (f result)

-- any such functor instance should follow the 2 functor laws
-- 1st law:
plain = parse parseByte L.empty           -- Left "byte offset 0: no more input"
withId = parse (id <$> parseByte) L.empty -- Left "byte offset 0: no more input"

inWord = L8.pack "foo"
plain2 = parse parseByte inWord           -- Right 102
withId2 = parse (id <$> parseByte) inWord -- Right 102

-- 2nd law: (Composability)
lhsParseLaw2 = parse ((chr . fromIntegral) <$> parseByte) inWord
rhsParseLaw2 = parse (chr <$> fromIntegral <$> parseByte) inWord

-- Using functors for parsing
-- Let us say we want to write the parseChar function similar to parseByte
-- We need not duplicate the same, and make use of fmap
-- The functor takes the result of the parse and applies a function (we can
-- as well convert from Byte to Char)
w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

-- Using functors to "peek"
-- Look at what the byte is without actually consuming it
-- Nothing when at the end of the string
-- remember 'string' is an accessor function in ParseState definition
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState
-- here also two fmap lifting (using fmap and <$>), due to Parse and Maybe

-- Similarly we can write a peekChar using the fmap lifting
peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte    -- Two lifting because of Parse and Maybe

-- writing parseWhile, analogous to takeWhile
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []

-- same function, without using functors
parseWhileVerbose :: (Word8 -> Bool) -> Parse [Word8]
parseWhileVerbose p =
    peekByte ==> \mc ->
    case mc of
        Nothing -> identity []
        Just c | p c ->
                   parseByte ==> \b ->
                   parseWhileVerbose p ==> \bs ->
                   identity (b:bs)
               | otherwise -> 
                   identity []
                   
-- Now, rewriting the PGM Parser
parseRawPGM = 
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` "\r\n\t")
  
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

-- This ==>& is same as ==>, except that right hand side ignores the result of
-- the left.  So one parsing done, followed by another without looking what the
-- first parse has given as result                   
(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

-- parseP5 kept passing the tuples 
-- parseRawPGM has kept the parsing state completely hidden

-- inspecting and modifying the parsing state is done using parseBytes function
parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h
       