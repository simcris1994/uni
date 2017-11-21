-- Programming Paradigms 2nd miniproject - a Haskell package for univariate expressions

import Data.Char
import Parse

-- types for X (main variable), C (a constant), K (a power)
type X = String
type C = Double
type K = Integer

-- data type for a polynomial - can be a variable, a polynomial multiplied with a constant,
-- two polynomials added, a variable raised to a power k
data P = Var X | Times C P | Plus P P | Power X K deriving (Eq, Show)

-- data type for an expression, can be a polynomial, the addition of two expressions,
-- the multiplication of two expressions, the division of two expressions,
-- the derivative of a function (expression) with respect to a variable, or the sine
-- or cosine of an expression
data E = Pol P | Add E E | Mult E E | Divide E E | Der E | Sin E | Cos E deriving (Eq, Show)

-- function that converts an expression formatted as a string into an expression with the Exp data type
-- uses the lexer and the parser generated with Happy
run :: String -> Exp
run = parser . lexer


-- function that converts an expression from the intermediary Exp type to the final E type
convertExp :: Exp -> E
convertExp (PlusE e1 e2) = (Add (convertExp e1) (convertExp e2))
convertExp (MinusE e1 e2) = (Add (convertExp e1) (Mult (numConst (-1) "x") (convertExp e2)))
convertExp (SinE e) = (Sin (convertExp e))
convertExp (CosE e) = (Cos (convertExp e))
convertExp (DerE e) = (Der (convertExp e))
convertExp (Term t) = convertTerm t

convertTerm :: Term -> E
convertTerm (TimesE t1 t2) = (Mult (convertTerm t1) (convertTerm t2))
convertTerm (DivE t1 t2) = (Divide (convertTerm t1) (convertTerm t2))
convertTerm (Factor f) = convertFactor f

convertFactor :: Factor -> E
convertFactor (Int i) = (numConst (fromIntegral i) "x")
convertFactor (VarE str) = (Pol (Var str))
convertFactor (Brack e) = convertExp e
convertFactor (PowerE e i) = (powerToMult (convertExp e) (i - 1) (convertExp e))

powerToMult e 0 res = res
powerToMult e n res = powerToMult e (n - 1) (Mult e res)



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
ex5 = Add (Add (powerOf "x" 7) (powerOf "x" 3)) (numConst 7 "x")
ex6 = Mult (Mult (powerOf "x" 2) (powerOf "x" 4)) (powerOf "x" 3)
ex7 = Mult (numConst 7 "x") (Add (powerOf "x" 5) (powerOf "x" 3))
ex8 = Mult (powerOf "x" 5) (numConst 1 "x")

t1 = Add (numConst 5 "x") (numConst 3 "x")
t2 = Mult (numConst 7 "x") (numConst 8 "x")
t3 = Divide (numConst 50 "x") (numConst 5 "x")
t4 = 0
t5 = numConst 7 "x"
t6 = Divide (Mult (Mult (numConst 1 "x") (Add (sin2 "x") (cos2 "x"))) (powerOf "x" 3)) (powerOf "x" 2)
t7 = (Der (Pol (Power "x" 2)))
t8 = (Der (Add (powerOf "x" 4) (Der (cosine "x"))))

    
-- functions for evaluating an expression, uses pattern matching to cover different cases    
eval :: E -> E    
eval (Add e1 e2) = addE (eval e1) (eval e2)
eval (Mult e1 e2) =  multE (eval e1) (eval e2)
eval (Divide e1 e2) = divideE (eval e1) (eval e2)
eval (Der e) =  derE (eval e)
eval (Sin e) = Sin (eval e)
eval (Cos e) = Cos (eval e)
eval (Pol p) = Pol p

addE (Pol (Times a (Power b 0))) (Pol (Times c (Power d 0))) = numConst (a + c) "x"
addE (Mult (Sin (Pol (Var a))) (Sin (Pol (Var b)))) (Mult (Cos (Pol (Var x))) (Cos (Pol (Var y))))
    | a == b && x == y && a == y    =   numConst 1 x
    | otherwise                     =  (Add (Mult (Sin (Pol (Var a))) (Sin (Pol (Var b)))) (Mult (Cos (Pol (Var x))) (Cos (Pol (Var y)))))
--addE (Add x y) z = (Add x (Add y z))
--addE e1 e2 = (Add e1 e2)
addE e1 e2
    | e1 == e2  =   (Mult (Pol (Var "2")) e1)
    | otherwise =   (Add e1 e2)

multE (Pol (Times a (Power b 0))) (Pol (Times c (Power d 0))) = numConst (a * c) "x"
multE (Pol (Power x ex1)) (Pol (Power y ex2))
    | x == y    =   (Pol (Power x (ex1 + ex2)))
    | otherwise =   (Mult (Pol (Power x ex1)) (Pol (Power y ex2)))
multE (Pol (Var x)) (Pol (Var y))
    | x == y    =   (Pol (Power x 2))
    | otherwise =   (Mult (Pol (Var x)) (Pol (Var y)))
multE a (Pol (Times 1.0 (Power x 0))) = a
multE (Pol (Times 1.0 (Power x 0))) a = a
--multE (Mult x y) z = (Mult x (Mult y z))
multE a (Add b c) = (Add (Mult a b) (Mult a c))
multE e1 e2 = (Mult e1 e2)
--multE e1 e2 = (Mult e2 e1)

divideE (Pol (Times a (Power b 0))) (Pol (Times c (Power d 0))) = numConst (a / c) "x"
divideE (Pol (Power x ex1)) (Pol (Power y ex2))
    | x == y    =   (Pol (Power x (ex1 - ex2)))
    | otherwise = (Divide (Pol (Power x ex1)) (Pol (Power y ex2)))
divideE e1 e2 = (Divide e1 e2)

derE (Pol (Times a (Power b 0))) = numConst 0 "x"
derE (Pol (Var "x")) = numConst 1 "x"
derE (Pol (Times a (Var "x"))) = numConst a "x"
derE (Pol (Power "x" 2)) = (Pol (Times 2 (Power "x" 1)))
derE (Sin e) = Cos e
derE (Cos e) = Mult (numConst (-1) "x") (Sin e)
derE (Mult (Pol (Times a (Power "x" 0))) e) = (Mult (Pol (Times a (Power "x" 0))) (derE e))
derE (Pol (Power a n)) = (Mult (numConst (fromIntegral n) "x") (Pol (Power a (n - 1))))
derE (Add e1 e2) = (Add (derE e1) (derE e2))
derE (Mult e1 e2) = (Add (Mult e1 (derE e2)) (Mult (derE e1) e2))
derE (Divide (Pol (Power "x" 0)) e) = (Divide (Mult (numConst (-1) "x") (derE e)) (Mult e e))
derE (Divide e1 e2) = (Divide (Add (Mult (derE e1) e2) (Mult (numConst (-1) "x") (Mult (derE e2) e1))) (Mult e2 e2))


--test = (convertExp (run "(sin x) * (sin x) + (cos x) * (cos x)"))

main = do putStrLn "Input expression: "
          exp <- getLine
          print (eval (convertExp (run (exp))))
