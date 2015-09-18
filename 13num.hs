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
    in  pa ++ pop ++ pb
    
prettyShow (UnaryArith opstr a) = opstr ++ "(" ++ show a ++ ")"

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

-- Reverse Polish Notation (RPN)
rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i = 
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
        toList (UnaryArith op a)    = toList a ++ [op]
        join :: [a] -> [[a]] -> [a]
        join delim l = concat (intersperse delim l)
        in  join " " (toList i)
-- Such a succint implementation of a function to express in RPN
    
-- Simplification 
simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a 
simplify (BinaryArith op ia ib) =
    let sa = simplify ia
        sb = simplify ib
        in case (op, sa, sb) of
            (Mul, Number 1, b) -> b
            (Mul, a, Number 1) -> a
            (Mul, Number 0, b) -> Number 0
            (Mul, a, Number 0) -> Number 0
            (Div, a, Number 1) -> a
            (Plus, Number 0, b) -> b
            (Plus, a, Number 0) -> a
            (Minus, a, Number 0) -> a
            _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

-- Adding the support for units
-- Define a new data type.
data Units a = Units a (SymbolicManip a) deriving (Eq)

-- defining an instance of Num for Units
-- Add two quantities, if same units, else generate error
instance (Num a, Eq a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | (ua == ub) = Units (xa + xb) ua
        | otherwise  = error "Mismatched units in addition or subtraction"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

-- Making Units, an instance of Fractional
instance (Fractional a, Eq a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa /xb) (ua /ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)
    
-- Making Units an instance of Floating
-- Addition intelligence to support angles in degrees and radian    
instance (Floating a, Eq a) => Floating (Units a) where
    pi = (Units pi (Number 1))
    exp _ = error "Exp is not implemented in units"
    log _ = error "Log is not implemented in inits"
    (Units xa ua) ** (Units xb ub) 
        | ub == (Number 1) = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua) 
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "Units for sin must be deg or rad"
    cos (Units xa ua) 
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "Units for tan must be deg or rad"
    asin (Units xa ua) 
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "Units for acos must be empty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "Units for atan must be empty"
    sinh = error "sinh not yet implemented in Units"
    cosh = error "cosh not yet implemented in Units"
    tanh = error "tanh not yet implemented in Units"
    asinh = error "asinh not yet implemented in Units"
    acosh = error "acosh not yet implemented in Units"
    atanh = error "atanh not yet implemented in Units"
    
{- A simple function that takes a number and a String and returns an
appropriate Units type to represent the number and its unit of measure -}
units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)
-- Using the above function we can say 
-- units 5 "m" instead of Units 5 (Symbol "m")

{- Extract the number only out of a Units type -}
dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x
                                                    
{- Utilities for the Unit implementation -}
deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)

-- Show instance for units
instance (Show a, Num a, Eq a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)
    
-- a variable for testing
test :: (Num a) => a
test = 2 * 5 + 3
