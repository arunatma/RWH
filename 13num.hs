-- Example Code in Chapter 13 - Data Structures
-- http://book.realworldhaskell.org/read/data-structures.html

import Data.List

-- Symbolic Units Manipulation

-- Operators Supported
data Op = Plus | Minus | Mul | Div | Pow deriving (Eq, Show)

-- Symbolic Manipulation Type
data SymbolicManip a = 
        Number a
    |   Symbol String
    |   BinaryArith Op (SymbolicManip a) (SymbolicManip a)
    |   UnaryArith String (SymbolicManip a)
    deriving (Eq)
    
-- Making SymbolicManip an instance of Num
instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b 
    a * b = BinaryArith Mul a b 
    negate a    = BinaryArith Mul (Number (-1)) a 
    abs a       = UnaryArith "abs" a
    signum _    = error "signum is not implemented"
    fromInteger i = Number (fromInteger i)
    
-- the definition of "abs" above, is made possible by the UnaryArith data  
-- constructor (UnaryArith value constructor)

-- Making SymbolicManip an instance of Fractional
instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b 
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)
    
-- Making SymbolicManip an instance of Floating
instance (Floating a) => Floating (SymbolicManip a) where
    pi      = Symbol "pi"
    exp a   = UnaryArith "exp" a
    log a   = UnaryArith "log" a
    sqrt a  = UnaryArith "sqrt" a 
    a ** b  = BinaryArith Pow a b
    sin a   = UnaryArith "sin" a 
    cos a   = UnaryArith "cos" a 
    tan a   = UnaryArith "tan" a 
    asin a  = UnaryArith "asin" a 
    acos a  = UnaryArith "acos" a 
    atan a  = UnaryArith "atan" a 
    sinh a  = UnaryArith "sinh" a 
    cosh a  = UnaryArith "cosh" a 
    tanh a  = UnaryArith "tanh" a 
    asinh a = UnaryArith "asinh" a 
    acosh a = UnaryArith "acosh" a 
    atanh a = UnaryArith "atanh" a 

-- Converting expressions to strings for display
-- Implementing the instance for show
prettyShow :: (Show a, Num a) => (SymbolicManip a) -> String

prettyShow (Number x) = show x
prettyShow (Symbol x) = x

prettyShow (BinaryArith op a b) = 
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
    in  pa ++ pop + pb
    
prettyShow (UnaryArith optstr a) = opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus     = "+"
op2str Minus    = "-"
op2str Mul      = "*"
op2str Div      = "/"
op2str Pow      = "**"
    
-- Function to add parenthesis (used by prettyShow for BinaryArith data cons.)
simpleParen :: (Show a, Num a) => (SymbolicManip a) -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) =  "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow a

    