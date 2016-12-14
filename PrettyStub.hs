-- This file is just to keep checking the functions for types and proper
-- compilation, when building the library

-- It is a good practice to have such a file, for continuous trial & test

import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)
         
-- Make use of "undefined" which is of type "t"
string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

-- (<>) does something with taking 2 Doc and returning another Doc
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

-- char function converts a Char to Doc 
char :: Char -> Doc
char c = undefined

-- hcat: concatenates multiple Doc values to a single Doc
hcat :: [Doc] -> Doc
hcat xs = undefined

-- used by 'series' function in PrettyJSON.hs
fsep :: [Doc] -> Doc
fsep xs = undefined

-- Appends the punctuation, except for the last 'Doc' in the list.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds