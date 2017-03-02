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

-- Support for Units

-- Defining a new data type for Units
data Units a = Units a (SymbolicManip a) deriving (Eq)
-- Units contain a number and a label (the label is of SymbolicManip type)

instance (Eq a, Num a) => Num (Units a) where
    (Units xa ua) + (Units xb ub)
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add or subtract"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)
    
-- Making the units SymbolicManip instead of String solves the units handling
-- Now m * m automatically becomes "m * m" - no need to explicitly do any work

instance (Eq a, Fractional a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)
    
instance (Eq a, Floating a) => Floating (Units a) where
    pi = Units pi (Number 1)
    exp _ = error "exp is not implemented in units"
    log _ = error "log is not implemented in units"
    (Units xa ua) ** (Units xb ub)
        | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua)
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise          = error "Units for sin must be deg or rad"
    cos (Units xa ua)
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise          = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise          = error "Units for tan must be deg or rad"
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
    
-- Degree Radian conversions
deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)    

-- take a number and a string and return the same in "Units" data type
units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)

-- reverse converstion from Units to plain number
dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x

-- Show instance for Units
instance (Show a, Num a, Eq a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "::" ++ prettyShow (simplify ua)
    
