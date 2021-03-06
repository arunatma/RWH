{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Real World Haskell
-- Part of Chapter 15: Programming with Monads
-- http://book.realworldhaskell.org/read/programming-with-monads.html
-- HandleIO.hs

-- Hiding the IO Monad

-- IO Monad is extemely powerful - so, vulnerable to accidental errors!

-- Need: Access local files: read and write.  But do not access network 
-- So, need to restrict the IO Monad

module HandleIO 
    (
        HandleIO
    ,   Handle 
    ,   IOMode(..)
    ,   runHandleIO
    ,   openFile
    ,   hClose
    ,   hPutStrLn
    )   where
    
import System.IO (Handle, IOMode (..))      
    -- only the above fns can directly be used. All other needs to be used 
    -- with System.IO as those are qualified imports as below 
import qualified System.IO
import Control.Monad.Trans (MonadIO(..))
import System.Directory (removeFile)

-- restricted version of IO
-- wrap it with newtype
newtype HandleIO a = HandleIO {runHandleIO :: IO a}
    deriving (Functor, Applicative, Monad)
-- export the type constructor (HandleIO) and runHandleIO execution fn 
-- and not the data constructor (HandleIO)

{-
    ghci> :t runHandleIO
    runHandleIO :: HandleIO a -> IO a
    
    ghci> :t HandleIO
    HandleIO :: IO a -> HandleIO a    
-}
-- runHandleIO gets a (HandleIO a) and returns an (IO a)
-- HandleIO gets an (IO a) and returns a (HandleIO a)

-- wrap each of the allowed Monad actions 
-- wrap each IO with HandleIO data constructor
openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose        -- just wrap in HandleIO

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h
  
safeWrite :: FilePath -> String -> HandleIO ()
safeWrite path content = do
    h <- openFile path WriteMode
    hPutStrLn h content
    hClose h

{-  Executing using runHandleIO
    runHandleIO (safeWrite "helloWorld.txt" "HelloWorld! Written via HandleIO")    
    -- new file helloWorld.txt created with the given content    
-}
    
-- Try another operation which is not defined, does not work 
-- runHandleIO (safeHello "goodbye" >> removeFile "goodbye")
-- removeFile is part of HandleIO
-- That way, we will always be operating within HandleIO monad 

-- Designing for Unexpected Use cases 
-- How to take care of the instances when we actually need to access some 
-- functions of the IO?

-- Way to provide an escape path 
-- Using Control.Monad.Trans

-- Making HandleIO an instance of MonadIO typeclass (from Control.Monad.Trans)

instance MonadIO HandleIO where
    liftIO = HandleIO
    
-- with liftIO, we can invoke direct IO actions wherever needed
tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
    safeHello path
    liftIO (removeFile path)  
    
-- same as above: this works! because we used liftIO
-- runHandleIO (safeHello "goodbye" >> liftIO (removeFile "goodbye"))
        
