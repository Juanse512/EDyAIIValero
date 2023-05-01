import Data.Char (isDigit)
import Data.Char (digitToInt)
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

eval :: Exp -> Int
eval (Lit x) = x
eval (Add x1 x2) = (eval x1) + (eval x2)
eval (Sub x1 x2) = (eval x1) - (eval x2)
eval (Prod x1 x2) = (eval x1) * (eval x2)
eval (Div x1 x2) = (eval x1) `div` (eval x2)

parseEval :: Char -> Exp -> Exp -> Exp
parseEval '+' x1 x2 = Add x1 x2
parseEval '-' x1 x2 = Sub x2 x1
parseEval '*' x1 x2 = Prod x1 x2
parseEval '/' x1 x2 = Div x1 x2


parseRPNAux :: [Exp] -> String -> Exp
parseRPNAux e [x] = parseEval x (head e) (head (tail e))
parseRPNAux e (x:xs) = if isDigit x then parseRPNAux ((Lit (digitToInt x)):e) xs else parseRPNAux ((parseEval x (head e) (head (tail e))):(tail (tail e))) xs

parseRPN :: String -> Exp
parseRPN [x] = Lit (digitToInt x)
parseRPN x = parseRPNAux [] x

evalRPN :: String -> Int
evalRPN [] = 0
evalRPN x = eval (parseRPN x)