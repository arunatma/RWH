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
          
-- Making SymbolicManip an instance of Num
instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is not implemented"
    fromInteger i = Number (fromInteger i)
    
-- An instance of Fractional
instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)
    
-- An instance of Floating
instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a
    
-- To do a pretty show printing
prettyShow :: (Show a, Num a) => SymbolicManip a -> String

prettyShow (Number x) = show x
prettyShow (Symbol x) = x

prettyShow (BinaryArith op a b) = pa ++ pOp ++ pb
    where pa = simpleParen a
          pb = simpleParen b
          pOp = op2str op

prettyShow (UnaryArith opStr a) = opStr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

-- adding parenthesis (add converstatively, even when it may not be needed)
simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

-- defining own Show instance for SymbolicManip
instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow a          
    
-- Converting an expression to RPN - Reverse Polish Notation
rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i = join " " (toList i)
    where join :: [a] -> [[a]] -> [a]
          join delim l = concat (intersperse delim l)
          
          toList (Number x) = [show x]
          toList (Symbol x) = [x]
          toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
          toList (UnaryArith op a) = toList a ++ [op]
          
-- Simplification of expressions Ex: (1 * 3) is 3
simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) = 
    let sa = simplify ia
        sb = simplify ib
    in 
    case (op, sa, sb) of 
        (Mul, Number 1, b) -> b
        (Mul, a, Number 1) -> a
        (Mul, Number 0, b) -> Number 0
        (Mul, a, Number 0) -> Number 0
        (Div, a, Number 1) -> a
        (Plus, a, Number 0) -> a
        (Plus, Number 0, b) -> b
        (Minus, a, Number 0) -> a
        _ -> BinaryArith op sa sb
        
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

