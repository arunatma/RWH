-- ch02.hs
-- Chapter 2. Types and Functions   
-- http://book.realworldhaskell.org/read/types-and-functions.html

add a b = a + b

myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop n (x:xs)
    | n > 0 = myDrop (n-1) xs
    | otherwise = x:xs

-- Implementation as provided in RWH book
myDrop' n xs = if n <= 0 || null xs
              then xs
              else myDrop' (n - 1) (tail xs)    
              
              
lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne [x] = Nothing
lastButOne (x:y:xs) = Just y

