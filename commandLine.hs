import System.Environment (getProgName, getArgs, getEnv, getEnvironment)

main = do 
        progName <- getProgName
        args <- getArgs 
        putStrLn ("Haskell File Name: " ++ progName)
        putStrLn ("Invoked with Arguments: " ++ (unlines args))
        putStrLn "Type out an environment variable, like USERPROFILE"
        envVar <- getLine
        envVal <- getEnv envVar
        putStrLn (envVar ++ " :: " ++ envVal)
        
-- from ghci interactive :main or main can be used to run a main program.
