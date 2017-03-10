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

-- Understanding List Monad
-- List Comprehension
comprehensive xs ys = [(x,y) | x <- xs, y <- ys]

-- same in terms of monadic representation 
monadic xs ys = do { x <- xs; y <- ys; return (x,y)}

-- both are same 
boolCheck = comprehensive [1,2] "bar" == monadic [1,2] "bar"    -- True

-- same as monadic written as a do block, instead of a single statement
blockyMonadic xs ys = do 
    x <- xs
    y <- ys
    return (x, y)
    
{-     
    *Main> blockyMonadic [1,2] [3,4]
    [(1,3),(1,4),(2,3),(2,4)]
-}    
    
    
-- So, in the functions above, x <- xs, takes each value of x, and y <- ys 
-- takes each value of y combines as (x,y) and returns
-- This works as a double nested loop!

-- So, monadic code is NOT SEQUENTIAL.  You will never be able to predict
-- what happens when, when the values are assigned!

-- This is what actually happens!
blockyPlain xs ys =
    xs >>=
    \x -> ys >>=        -- For each value of x, y gets multiple values 
    \y -> return (x, y)

-- with no bind operator    
noMonadAtAll xs ys =
    concat (map 
           (\x -> concat (map 
                          (\y -> [(x, y)]) 
                          ys)) 
            xs)

-- Brute force constraint solver
-- Given an integer value, find all possible pairs of integers, when multiplied
-- gives the value 
guarded :: Bool -> [a] -> [a]
guarded True xs = xs
guarded False _ = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
    x <- [1..n]
    y <- [1..n]
    guarded (x * y == n) $ 
        return (x, y)
    
-- use of both list and maybe     
getSecondElem :: [a] -> Maybe a
getSecondElem xs = do 
    (_:x:_) <- Just xs      -- executing this on [1] causes a pattern match 
    return x                -- fail, which in Maybe monad gives "Nothing"
    
-- Desugaring of "do" blocks    
-- Refer to the five translations in this section. (Go to book)   
-- READ THIS.  Remember this for a Monad!
-- When we write (>>=) explicitly in our code, it reminds us that we're 
-- stitching functions together using combinators, not simply sequencing actions

-- (=<<) is flipped version of (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b

wordCount = print . length . words =<< getContents

-- State Monad 
-- Already an introuduction happened in chapter 10.
-- "Parse" data type while parsing binary data was a monad.
-- Either "error message" state - Left error and Right parse state 
{-
    data ParseState = ParseState {
          string :: L.ByteString
        , offset :: Int64
        } deriving (Show)

    newtype Parse a = Parse {
        runParse :: ParseState -> Either String (a, ParseState)
    }
-}
-- The reading and writing state and performing state operations is provided by 
-- "State" monad from Control.Monad.State

-- Parse carried "ByteString" as state, but State monad can carry any type 'a'
-- as the state 

-- What to do with a State?
-- Given a state
--  1. inspect into it
--  2. produce a result 
--  3. produce a new state value 
-- Type:
-- s -> (a, s)              (old state) -> (result, new state)

-- Developing a simple state monad 
type SimpleState s a = s -> (a, s)
-- SimpleState s a is a function, that takes a state and gives a result and 
-- new state.
-- because it transforms one state to another, it is also called "state 
-- transformer" moad

-- Right, any monad should have a single type parameter, but here there are 
-- two, 's' and 'a'. So, technically SimpleState cannot form a monad.

type StringState a = SimpleState String a
-- (so, the monad's type constructor is SimpleState s and not SimpleState)

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)
-- Given a value, returnSt gives out a fn, that takes a state and gives (a, s)

-- Effectively, given a value and a state, put in tuple and give back!!
returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)
-- just comment out the type declaration above, we would get type of returnAlt 
-- as returnAlt :: t1 -> t -> (t1, t)

-- definition for bind (>>=)
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> (SimpleState s b)
bindSt m k = \s -> let (a, s') = m s 
                   in (k a) s'

-- m is (SimpleState s a)
-- m is (s -> (a, s))
-- m s will actually give (a, s)        (with some transformation to state)

-- k is a fn of type (a -> SimpleState s b)
-- k a will yield (SimpleState s b)
-- k a, thus actually is (s -> (b, s))
-- (k a) s' will then be (b, s')        (with some transformation to state)

-- bindSt m k = \s -> (b, s')  (a function that takes in a state and produces
-- a tuple) which is exactly the type of (SimpleState s b)

-- To explain further:
-- m: step
-- k: makeStep 
-- s: oldState

bindAlt :: (s -> (a, s))        -- step
        -> (a -> s -> (b, s))   -- makeStep
        -> (s -> (b, s))        -- (makeStep result) newState
bindAlt step makeStep oldState = let (result, newState) = step oldState
                                 in (makeStep result) newState

-- In the above definition, how we have done is                                  
{-
bindAlt :: (s -> (a, s))        -- step         m 
        -> (a -> s -> (b, s))   -- makeStep     k
        -> s                    -- oldState     s 
        (b, s)                  
commenting out the type declaration for bindAlt gives the above type!
bindAlt :: (s -> (a, s)) -> (a -> s -> t) -> s -> t
            t here is (b, s)
-}                                 

-- Reading and Modifying the state 
-- using getSt and putSt functions
getSt :: SimpleState s s
getSt = \s -> (s, s)
-- getState oldState gives (oldState, oldState)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
-- putSt newState oldState gives ((), newState)

-- Instead of using the type synonmy we are going to define a newtype
newtype State s a = State {
      runState :: s -> (a, s)
    }
-- instead of plainly leaving (s -> (a, s) we have wrapped it with the "State" 
-- value constructor
-- runState :: State s a -> s -> (a, s)
-- runState accessor function unwraps the State value (s -> (a, s)) from the 
-- type constructor State s a
-- runState (State s a) oldState = (result, newState)

-- now definitions of return and bind
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)    
-- same as returnSt, just put inside 'State' type constructor

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s' 

-- get and put
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
    