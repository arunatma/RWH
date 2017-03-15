-- Real World Haskell
-- Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html

import Control.Monad (liftM3)

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

