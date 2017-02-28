-- Part of Chapter 13
-- Chapter 13: Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

import Data.List

-- Symbolic Manipulation
-- Support for Units 
-- Support for Trigonometry

-- Operators that are being supported
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)
        
-- Core Symbolic Manipulation Data Type
-- Can be a: Number or a Binary Arith Operation or a Unary Arith Operation

data SymbolicManip a =
          Number a              -- Simple number such as '7'
        | Symbol String         -- Algebraic variables such as 'x'
        | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
        | UnaryArith String (SymbolicManip a)
          deriving (Eq)
          
          