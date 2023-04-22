data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show


eval :: Exp -> Int
eval (Lit x) = x
eval (Add x1 x2) = (eval x1) + (eval x2)
eval (Sub x1 x2) = (eval x1) - (eval x2)
eval (Prod x1 x2) = (eval x1) * (eval x2)
eval (Div x1 x2) = (eval x1) `div` (eval x2)
