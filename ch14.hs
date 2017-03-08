-- Real World Haskell
-- Chapter 14: Monads
-- http://book.realworldhaskell.org/read/monads.html

-- A relook from previous chapters

import qualified Data.Map as M

-- Chapter 10 - PNM bitstream parsing
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

-- Parse using ParseState
{-
(==>) :: Parse a -> (a -> Parse b) -> Parse b

firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
-}

-- How a Maybe is related to Parse?
{-
    data Maybe a = Nothing
                 | Just a

    newtype Parse a = Parse {
        runParse :: ParseState -> Either String (a, ParseState)
    }
-}

-- Both has single type parameter 'a' which appears on right side
-- ==> and ==? types look very similar!

-- Monads need to have exactly 3 properties
{-
    1. A type constructor 'm'
    2. A chain function of the following type: (chaining output of one to input  
       of another)
        chain :: m a -> (a -> m b) -> m b
    3. An inject function of type: (wraps a value 'a' with type constructor 'm')
        inject :: a -> m a
        (identity fn takes this role for our user defined 'Parse' from ch10) 
       
-}

-- Monad type class (defined in Prelude)
{-
    class Monad m where
        -- chain
        (>>=)  :: m a -> (a -> m b) -> m b      -- (called bind function)
        -- inject
        return :: a -> m a
        -- chaining ignoring result of the first
        (>>) :: m a -> m b -> m b
        a >> f = a >>= \_ -> f                  -- default implementation for >>
        fail :: String -> m a
        fail = error                            -- not generally used    
-}

chainEx1 = print "foo" >>= \_ -> print "bar"
chainEx2 = print "foo" >> print "bar"
-- both of above perform same task

-- Having a record log, using monads as our helper
-- back to globToRegex function from Chapter 8: Efficient file processing

-- This logger code is in Logger.hs 

-- Maybe Monad 
{-
    Defined in GHC.Base
    
    instance Monad Maybe where
        Just x >>= k    = k x
        Nothing >>= _   = Nothing
        
        Just _ >> k     = k
        Nothing >> _    = Nothing
        
        return x        = Just x
        
        fail _          = Nothing
        
    Executing the Maybe Monad using "maybe" function 
    
    maybe :: b -> (a -> b) -> Maybe a -> b
    maybe n _ Nothing  = n
    maybe _ f (Just x) = f x
    
    Usage:  maybe defaultValue fn MonadicValue
        Output: defaultValue   when MonadicValue is Nothing
                fn (value)     when MonadicValue has some actual value
-}

type PersonName = String
type PhoneNumber = String
type BillingAddress = String

data MobileCarrier = AirVoice | Pio | Ideal | Medafone deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName 
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
                          
findCarrierBillingAddress person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap

-- variation2 using Maybe Monad 
findCarrierBillingAddress1 person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    address <- M.lookup carrier addressMap      -- why take out from monadic 
    return address                              -- context, to put back again!
    
-- without using the last <- and return     
findCarrierBillingAddress1' person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    M.lookup carrier addressMap    
    
-- using (>>=)
findCarrierBillingAddress2 person phoneMap carrierMap addressMap = 
    lookup phoneMap person >>=
    lookup carrierMap >>=
    lookup addressMap   
    where lookup = flip M.lookup

-- List Monad
-- Maybe is for just a single value or no value 
-- List is to give out a number of values as output. Non-deterministic!

-- type constructor here: []
-- return should put the value in the type constructor a -> [a] (or a -> [] a)

returnSingleton :: a -> [a]
returnSingleton x = [x]

-- type of (>>=)
-- (>>=) :: m a -> (a -> m b) -> m b 
-- (>>=) :: [a] -> (a -> [b]) -> [b]

-- Deriving (>>=) from the known list function
{-
    map :: (a -> b) -> [a] -> [b]
    flip map :: [a] -> (a -> b) -> [b]
    
    what if the type of b is actually [c]
    
    flip map :: [a] -> (a -> [c]) -> [[c]]
    \xs f -> concat (map f xs) :: [a] -> (a -> [b]) -> [b]
    
    (>>=) xs f == concat $ flip map xs f
-}

-- Monad Instance for List 
{-
    instance Monad [] where
        return x = [x]
        xs >>= f = concat (map f xs)
        
        xs >> f  = concat (map (\_ -> f) xs)
        fail _   = []
        
-}

