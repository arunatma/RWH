-- get an input from command line, print it out
-- getLine will give IO String, putStrLn takes it out of context as String 
-- and prints out to console
main = getLine >>= putStrLn

-- Running instructions:
-- 1. > runghc getBindPut.hs
-- 2 type getBindPut.hs | runghc getBindPut.hs 
--        (first line of this file gets printed)
