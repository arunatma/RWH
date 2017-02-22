-- Real World Haskell
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

-- Association Lists
-- Many instances need the use of key-value pairs
-- Association Lists and Data.Map help there
-- Map has considerable performance advantage over association lists
-- Association lists are simple and works like normal lists
-- This is just a list containing tuple of (key, value) pairs :: [(key, value)]

-- lookup (defined in Data.List) function: given a key, gets a value in Maybe
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
assocList = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
val1 = lookup 1 assocList       -- Just "one"
val2 = lookup 2 assocList       -- Just "two"

-- writing a lookup function
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup i ((k,v) : kvs)
    | i == k    = Just v
    | otherwise = myLookup i kvs 
myLookup i _    = Nothing

-- parsing an /etc/passwd file in a unix system
-- this file contains usernames, UIDs, home directories etc 
-- see passwd-al.hs

