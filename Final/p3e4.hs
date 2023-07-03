import Data.Char
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show
eval :: Exp -> Int
eval (Lit v) = v
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Prod a b) = (eval a) * (eval b)
eval (Div a b) = div (eval a) (eval b)

newEval :: Char -> Exp -> Exp -> Exp
newEval x a b | x == '+' = Add a b
              | x == '-' = Sub a b
              | x == '*' = Prod a b
              | x == '/' = Div a b

rpn :: String -> [Exp] -> [Exp]
rpn [] e = e
rpn (x:xs) e = if (isDigit x) then rpn xs ((Lit (digitToInt x)):e) else rpn xs ((newEval x (head e) (head (tail e))):(tail (tail e)))