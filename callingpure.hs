-- IO Action
-- type IO t
-- Produce an effect when performed, but not when evaluated
-- Performing (executing) an action of type IO t may perform I/O and will 
-- ultimately deliver a result of type t

name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

main :: IO ()
main = do
       putStrLn "Greetings once again.  What is your name?"
       inpStr <- getLine
       let outStr = name2reply inpStr       -- (note: No "let .. in" only "let")
       putStrLn outStr
