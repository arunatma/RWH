-- Real World Haskell
-- Chapter 14: Monads
-- http://book.realworldhaskell.org/read/monads.html

-- A relook from previous chapters

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

    
    