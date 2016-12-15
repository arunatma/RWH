-- Help functions for pretty printing
module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line             -- for the line breaks
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show,Eq)
           
-- Appends the punctuation, except for the last 'Doc' in the list.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- construction functions for each of different types to get 'Doc' type
empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = x `Concat` y


-- get all the individual elements in list and concatenate!
concat :: [[a]] -> [a]
concat = foldr (++) []

-- Doc concatenation
hcat :: [Doc] -> Doc
hcat = fold (<>)            -- We need to define fold function!

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- Or hcat could have been simply written as follows
-- hcat = foldr (<>) Empty

-- concatenate, inserting soft lines in between.
fsep :: [Doc] -> Doc
fsep = fold (</>)

-- inserting a softline
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

-- inserts a newline if the current line becomes too wide
softline :: Doc
softline = group line

-- two alternative versions; one flattened and one normal
group :: Doc -> Doc
group x = flatten x `Union` x

-- flatten replace new lines with space 
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x          -- always flatten the left
flatten other          = other


-- compact rendering - remove unnecessary white spaces
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)


{-                
    let value = renderJValue (JObject [("f", JNumber 1), ("q", JBool True)])
    :type value
    value :: Doc
    value
    
Concat (Concat (Char '{') (Concat (Concat (Concat (Concat (Concat (Concat (Conca
t (Char '"') (Char 'f')) (Char '"')) (Text ": ")) (Text "1.0")) (Char ',')) (Uni
on (Char ' ') Line)) (Concat (Concat (Concat (Concat (Concat (Char '"') (Char 'q
')) (Char '"')) (Text ": ")) (Text "true")) (Union (Char ' ') Line)))) (Char '}'
)
    
    putStrLn (compact value)
    {"f": 1.0,
    "q": true
    }
-}

-- Easy understanding of the code, using simpler example
{-

ghci> char 'f' <> text "oo"
Concat (Char 'f') (Text "oo")
ghci> compact (char 'f' <> text "oo")
"foo"

The internal working!!
======================
-- compact (char 'f' <> Text "oo")
-- compact (Concat (Char 'f') (Text "oo"))
-- transform [Concat (Char 'f') (Text "oo")]
-- transform [(Char 'f') `Concat` (Text "oo")]
-- transform (Char 'f') `Concat` (Text "oo") : []
-- transform (Char 'f' : Text "oo" : [])
-- 'f' : transform (Text "oo" : [])
-- 'f' : ("oo" + transform [])
-- 'f' : ("oo" ++ "")
-- 'f' : ("oo")
-- "foo"

-}

-- just the inverse of compact, which is for machine-readable, to remove extra
-- spaces and unnecessary white space to conserve bandwidth over communication
-- pretty prints the JSON file which is readable to human being pleasing to eye
-- pretty takes additional argument, an integer - the max width of a line.
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          -- select either a or b, on which fits better
          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

