-- file filter
-- read a file and print out every line that has the character 'a'
main = interact (unlines . filter (elem 'a') . lines)

{-

 $ runghc filter.hs < input.txt
  I like Haskell
  Haskell is great

 > type fileFilter.hs | runghc fileFilter.hs
 
-}
  