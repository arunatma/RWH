{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-- Real World Haskell
-- Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html

import Control.Monad (liftM3, ap, liftM)
import Control.Arrow (first, second)

-- Revisits:
-- Association List
movieAL = [("name", Just "Attila \"The Hun\""),
           ("occupation", Just "Khan")]

-- storing a movie review from the HTTP
data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
    }
    
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist = 
    case lookup "title" alist of
        Just (Just title@(_:_)) ->
            case lookup "user" alist of
                Just (Just user@(_:_)) ->
                    case lookup "review" alist of 
                        Just (Just review@(_:_)) ->
                            Just (MovieReview title user review)
                        _ -> Nothing        -- no review
                _ -> Nothing                -- no user
        _ -> Nothing                        -- no title 
        
-- MovieReview is obtained only if the alist contains all the three values

-- Now, the same using Maybe monad
maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview alist = do
    title  <- lookup1 "title"  alist
    user   <- lookup1 "user"   alist
    review <- lookup1 "review" alist
    return (MovieReview title user review)
    
lookup1 key alist = case lookup key alist of
                        Just (Just s@(_:_)) -> Just s
                        _ -> Nothing
                        
-- Now, the same using liftM
liftedReview :: [(String, Maybe String)] -> Maybe MovieReview
liftedReview alist = 
    liftM3 MovieReview (lookup1 "title"  alist)
                       (lookup1 "user"   alist)
                       (lookup1 "review" alist)
                       
-- liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
-- liftM3 kind of function is not generic. It takes a function and three monads
-- values and returns a monad.  The function is a plain function

-- This liftM3 kind of function is available only till liftM5. This will have
-- a function taking 5 plain inputs. 5 Monads and returns a monad
{-
    liftM5 :: Monad m =>
         (a1 -> a2 -> a3 -> a4 -> a5 -> r)
         -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
-}

-- So, there is a function 'ap' that will enable us to write a generic lift
{-
       ap :: Monad m       => m (a -> b) -> m a -> m b
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-}
-- It is the same applicative function which provides a basic lift over fn

-- now, the liftedReview fn can be transformed to
apReview alist =
    MovieReview `liftM` lookup1 "title" alist
                   `ap` lookup1 "user" alist
                   `ap` lookup1 "review" alist

-- Explanation:
-- MovieReview :: String -> String -> String -> MovieReview
-- (liftM MovieReview) :: Monad m => 
--           m String -> m (String -> String -> MovieReview)
-- So, this takes a string within a monad and returns a function in monad
-- That's exactly needed as the input for 'ap' function
-- (MovieReview `liftM` lookup1 "title" alist) :: 
--      Maybe (String -> String -> MovieReview)

-- 'ap' function 
-- same as the (<*>) function
-- Monadic equivalent of ($) function
-- ($) ::              (a -> b) ->   a ->   b
--  ap :: Monad m => m (a -> b) -> m a -> m b

{-
Very interesting equivalent functions!

    ap == liftM id
    
    ap == liftM ($)

-}

-- Representation of phone numbers
data Context = Home | Mobile | Business deriving (Show, Eq)

type Phone = String

albulena = [(Home, "+355-652-55512")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]

-- Order of Priority to call 1. Business, 2. Home, 3. Mobile
-- to get personal number, check home, if not then mobile
onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just n -> Just n
                        
-- Maybe does not accommodate a single person having multiple phone numbers 
-- in the same context (multiple "Business" phones)
allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
    where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns

contextIs a (b, _) = a == b

-- both the above functions look very similar
-- the first case expression handles the empty case 
-- the second case expression handles when there is a valid output

-- MonadPlus typeclass (using this we can abstract the above case expressions)
-- from Control.Monad
-- defined as:

class Monad m => MonadPlus m where
   mzero :: m a    
   mplus :: m a -> m a -> m a

-- Making instances for our usual data types 
instance MonadPlus [] where 
    mzero = []
    mplus = (++)
    
instance MonadPlus Maybe where
    mzero = Nothing
    
    -- Take only the first argument of mplus. If it is Nothing, then take second
    Nothing `mplus` ys  = ys
    xs      `mplus` _ = xs
    
-- rewritten functions
onePersonalPhone' :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone' ps = lookup Home ps `mplus` lookup Mobile ps

allBusinessPhones' :: [(Context, Phone)] -> [Phone]
allBusinessPhones' ps = map snd numbers
    where numbers = filter (contextIs Business) ps `mplus` 
                    filter (contextIs Mobile) ps
    
oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps

-- lookup function as defined in Data.List
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- Rewriting the same here
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ []    = Nothing
lookup' k ((x,y):xys) 
    | x == k    = Just y
    | otherwise = lookup' k xys
                      
-- A generic lookup can be written using MonadPlus
-- This can return a Maybe or this can return a list 
lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ []    = mzero
lookupM k ((x,y):xys) 
    | x == k    = return y `mplus` lookupM k xys
    | otherwise = lookupM k xys
    
-- Now, see the magic using lookupM
{-
    *Main> lookupM Business nils :: Maybe Phone
    Just "+47-922-12-121"
    *Main> lookupM Business nils :: [Phone]
    ["+47-922-12-121","+47-922-25-551"]
-}    

-- mplus does not mean addition
mplusEx1 = [1,2,3] `mplus` [4,5,6]          -- [1,2,3,4,5,6]
mplusEx2 = Just 1 `mplus` Just 2            -- Just 1

-- Rules for working with MonadPlus
-- mzero short circuits the chain of actions
-- 1. mzero >>= f == mzero
-- 2. v >> mzero  == mzero

-- Failing safely with MonadPlus
-- use mzero in case of a fail or error with Monad

-- guard fn in Control.Monad (not imported, so possible to define below)
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- a sample function using guard
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x

{-
    *Main> zeroMod 5 2 :: [Int]
    []
    *Main> zeroMod 4 2 :: [Int]
    [4]
-}

-- Back to State monad and generating random values 
-- the implementation was "leaky" - users can inspect and modify the state!!
-- We want to hide the implementation.
-- Such kind of meddling with the implementation can be a source of hard-to-find
-- bugs - no one would doubt the implementation!
-- So, better hide the implementation. Expose the APIs / fns which are necssy.

-- implement a monad that supplies unique value of any kind
-- see Supply.hs 

-- Using Supply monad as a source of random numbers 
-- see RandomSupply.hs

-- There is a tuple.  (a, b) I need to apply a fn to only one of them
-- So the result should be (f a, b) or (a, f b)
-- first and second fns from Control.Arrow helps!

theTuple = (5, 3)
modTuple1 = first (+10) theTuple        -- (15, 3)
modTuple2 = second (even) theTuple      -- (5, False)

-- see RandomSupply.hs
-- we are using System.Random functions to generate random numbers
-- These functions are slow.
-- What if we want to use randomsIO, but not internally use the System.Random
-- functions?

-- Separating interface from implementation
class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)
-- (Use MultiParamTypeClasses to allow multi-parameter classes)
--      because of MonadSupply s m
--      (MonadSupply s) m
--      We are making 'm' an instance of (MonadSupply s)
--      A normal typeclass does not have a parameter, here MonadSupply has 's'

-- (Use FunctionalDependencies to allow fundeps)
--      because of  | m -> s (a functional dependency)
--      |  : such that
--      -> : uniquely determines
--      When 'm' is used in context of (MonadSupply s), then 's' is the only 
--      acceptable type to use with it 

-- Further explanation: see SupplyClass.hs
-- check for the function showTwo in Supply.hs
-- check for the equivalent fn showTwo_class in SupplyClass.hs

-- The Reader Monad
-- Need: A function accepting an environment (e) and returning a value (a)
newtype Reader e a = R {runReader :: e -> a} deriving (Functor, Applicative)
-- needs to have GeneralizedNewtypeDeriving language pragma.

-- if not for GeneralizedNewtypeDeriving, we can write Functor and Applicative
-- instance as below
{-
    instance Functor (Reader e) where
        fmap f mv = do
            v <- mv
            return (f v)
            
    instance Applicative (Reader e) where
        pure = return
        (<*>) mf mv = do
            f <- mf
            v <- mv
            return (f v)
-}

-- Making this a monad instance
instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r
    
-- How to know the piece of code executing in this monad, what's the env?
ask :: Reader e e
ask = R id      -- equivalent of ask x = R id x 
                -- runReader on this will give x
                -- runReader :: Reader e a -> e -> a
                
readerEx1 = runReader (ask >>= \x -> return (x * 3)) 2

-- reader monad is included in mtl library (Control.Monad.Reader)

-- See SupplyInstance.hs

-- Hiding the IO Monad
-- See HandleIO.hs

-- Using the type classes 
-- See MonadHandle.hs, MonadHandleIO.hs and safeHello.hs

-- Using the type class, we can swap one monad for another. The code retouch 
-- will be minimal as the code does not care about what monad the code is 
-- executing in


