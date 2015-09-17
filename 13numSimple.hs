-- Example Code in Chapter 13 - Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html


-- Extended Example: Numeric Types
-- Playing around with custom number types
-- With options to represent expressions, simplify expressions, including units
-- Enabling Reverse Polish Notation

-- FIRST ATTEMPT
-- A Simpler Version

-- Operators supported 
data Op = Plus | Minus | Mul | Div | Pow deriving (Eq, Show)

-- SymbolicManip data type definition
data SymbolicManip a = Number a | Arith Op (SymbolicManip a) (SymbolicManip a)
    deriving (Eq, Show)
-- (note above, that the Arith data type constructor is recursive)    
-- SymbolicManip is a parametric type
-- Like, a Maybe Int, Maybe Char, this should be SymbolicManip Int, SymbolicManip Float
    
-- Make SymbolicManip an instance of Num
-- Num mandates certain functions are to be defined for its instances

instance Num a => Num (SymbolicManip a) where
    a + b = Arith Plus a b
    a - b = Arith Minus a b 
    a * b = Arith Mul a b 
    negate a        = Arith Mul (Number (-1)) a
    abs a           = error "abs is not implemented"
    signum _        = error "signum is not implemented"
    fromInteger i   = Number (fromInteger i)
    
    
--------------------------------------------------------------------------------
{-
ghci> :l 13numsimple.hs
[1 of 1] Compiling Main             ( numsimple.hs, interpreted )
Ok, modules loaded: Main.
ghci> Number 5
Number 5
ghci> :t Number 5
Number 5 :: (Num t) => SymbolicManip t
ghci> :t Number (5::Int)
Number (5::Int) :: SymbolicManip Int
ghci> Number 5 * Number 10
Arith Mul (Number 5) (Number 10)
ghci> (5 * 10)::SymbolicManip Int
Arith Mul (Number 5) (Number 10)
ghci> (5 * 10 + 2)::SymbolicManip Int
Arith Plus (Arith Mul (Number 5) (Number 10)) (Number 2)
-}
--------------------------------------------------------------------------------
    