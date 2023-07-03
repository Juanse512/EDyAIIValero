import Data.Char (isDigit)
import Data.Char (digitToInt)
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

parseOp :: Char -> Exp -> Exp -> Exp
parseOp '+' x1 x2 = Add x1 x2
parseOp '-' x1 x2 = Sub x2 x1
parseOp '*' x1 x2 = Prod x1 x2
parseOp '/' x1 x2 = Div x1 x2

eval :: Exp -> Int
eval (Lit x) = x
eval (Add x1 x2) = (eval x1) + (eval x2)
eval (Sub x1 x2) = (eval x1) - (eval x2)
eval (Prod x1 x2) = (eval x1) * (eval x2)
eval (Div x1 x2) = (eval x1) `div` (eval x2)


parseRPNAux :: String -> [Exp] -> Exp
parseRPNAux [c] (l:ll:ls) = parseOp c l ll
parseRPNAux (s:ss) l = if (isDigit s) then parseRPNAux ss ((Lit (digitToInt s)):l) else parseRPNAux ss (parseOp s (head l) ((head (tail l))):(tail (tail l)))

parseRPN :: String -> Exp
parseRPN [] = Lit 0
parseRPN s = parseRPNAux s []