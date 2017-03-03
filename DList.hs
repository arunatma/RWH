-- Part of Chapter 13
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

module DList
    (
      DList
    , fromList
    , toList
    , empty
    , append
    , cons
    , dfoldr
    ) where
    
newtype DList a = DL {
      unDL :: [a] -> [a]
    }

-- unDL is the function deconstructor: takes a difference list and gives out a 
-- a function operating on normal list 
-- "DL" is a function which takes a function of type [a] -> [a] and returns 
-- DList a

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

-- same append function; alternative representation
append' :: DList a -> DList a -> DList a
append' (DL xs) (DL ys) = DL (xs . ys)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []          -- remember xs here is a fn on list

empty :: DList a
empty = DL id

-- creating an equivalent of cons (:)
cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)
infixr `cons`

-- folding over DList
dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xs = foldr f z (toList xs)

-- map functionality 
dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty 
    where go x xs = cons (f x) xs
    
instance Functor DList where
    fmap = dmap
    
-- head is a constant time operation in normal list
-- the same head is a linear time operation in difference list, as the 
-- list has to be constructed before looking at what the first element is
safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
                    (y:_) -> Just y
                    _     -> Nothing
                    
instance Monoid (DList a) where
    mempty = empty
    mappend = append
    
mappendEx1 = toList (fromList [1,2] `mappend` fromList [3,4])

