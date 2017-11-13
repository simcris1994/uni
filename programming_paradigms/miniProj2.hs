-- Programming Paradigms 2nd miniproject - a Haskell package for univariate expressions


-- types for X (main variable), C (a constant), K (a power)
type X = String
type C = Integer
type K = Integer

-- data type for a polynomial - can be a variable, a polynomial multiplied with a constant,
-- two polynomials added, a variable raised to a power k
data P = Var X | Times C P | Plus P P | Power X K deriving (Eq, Show)

-- data type for an expression, can be a polynomial, the addition of two expressions,
-- the multiplication of two expressions, the division of two expressions, 
-- the derivative of a function (expression) with respect to a variable, or the sine 
-- or cosine of an expression
data E = Pol P | Add E E | Mult E E | Divide E E | D X E | Sin E | Cos E deriving (Eq, Show)

-- helper functions for building various expressions, such as x^k, sin x, sin^2 x etc.
powerOf a b = (Pol (Power a b))
sine a = (Sin (Pol (Var a)))
cosine a = (Cos (Pol (Var a)))
sin2 a = (Mult (sine a) (sine a))
cos2 a = (Mult (cosine a) (cosine a))
numConst a x = (Pol (Times a (Power x 0)))

-- examples to test on
ex1 = Mult (powerOf "x" 2) (powerOf "x" 3)
ex2 = Divide (powerOf "x" 6) (powerOf "x" 3)
ex3 = Add (sin2 "x") (cos2 "x")
ex4 = Mult (Pol (Var "x")) (Pol (Var "x"))
ex5 = (Add (Add (powerOf "x" 7) (powerOf "x" 3)) (numConst 7 "x"))
ex6 = (Mult (Mult (powerOf "x" 2) (powerOf "x" 4)) (powerOf "x" 3))
-- ex7
ex8 = (Mult (powerOf "x" 5) (numConst 1 "x"))

-- functions for evaluating an expression, uses pattern matching to cover different cases

-- x^m * x^n = x^(m+n)
eval (Mult (Pol (Power x ex1)) (Pol (Power y ex2)))
    | x == y    =   (Pol (Power x (ex1 + ex2)))
    | otherwise =   (Mult (Pol (Power x ex1)) (Pol (Power y ex2)))
    
-- x^m / x^m = x^(m-n)
eval (Divide (Pol (Power x ex1)) (Pol (Power y ex2)))
    | x == y    =   (Pol (Power x (ex1 - ex2)))
    | otherwise = (Divide (Pol (Power x ex1)) (Pol (Power y ex2)))

-- sin^2 x + cos^2 x = 1
eval (Add (Mult (Sin (Pol (Var a))) (Sin (Pol (Var b)))) (Mult (Cos (Pol (Var x))) (Cos (Pol (Var y)))))
    | x == y    =   numConst 1 x
    | otherwise =  (Add (Mult (Sin (Pol (Var a))) (Sin (Pol (Var b)))) (Mult (Cos (Pol (Var x))) (Cos (Pol (Var y))))) 

-- e1  + e2 = e2 + e1
eval (Add x y) 
    | x == y    =   (Mult (Pol (Var "2")) x)
    | otherwise =   (Add y x)
    
-- e * e = e^2
eval (Mult (Pol (Var x)) (Pol (Var y)))
    | x == y    =   (Pol (Power x 2))
    | otherwise =   (Mult (Pol (Var y)) (Pol (Var x)))
    
-- (e1 + e2) + e3 = e1 + (e2 + e3)
eval (Add (Add x y) z) = (Add x (Add y z))

-- e1 * e2 = e2 * e1
eval (Mult a b) = (Mult b a)

-- (e1 * e2) * e3 = e1 * (e2 * e3)
eval (Mult (Mult x y) z) = (Mult x (Mult y z))

-- e1 * (e2 + e3) = e1 * e2 + e1 * e3

-- e1 * 1 = e1
eval (Mult a (Pol (Power x 0))) = a

