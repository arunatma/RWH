-- try running like below in command line
-- > echo hello | runghc makeUpper.hs
import Data.Char(toUpper)

main :: IO ()
main = interact ((++) "Your data, in uppercase, is:\n\n" . 
                 map toUpper)
