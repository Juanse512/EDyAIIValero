data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

justAdd :: Maybe Int -> Maybe Int -> Maybe Int
justAdd (Just x) (Just y) = Just (x + y)
justAdd Nothing _ = Nothing
justAdd _ Nothing = Nothing

justSub :: Maybe Int -> Maybe Int -> Maybe Int
justSub (Just x) (Just y) = Just (x - y)
justSub Nothing _ = Nothing
justSub _ Nothing = Nothing

justProd :: Maybe Int -> Maybe Int -> Maybe Int
justProd (Just x) (Just y) = Just (x * y)
justProd Nothing _ = Nothing
justProd _ Nothing = Nothing

justDiv :: Maybe Int -> Maybe Int -> Maybe Int
justDiv (Just x) (Just 0) = Nothing
justDiv (Just x) (Just y) = Just (x `div` y)
justDiv Nothing _ = Nothing
justDiv _ Nothing = Nothing

eval :: Exp -> Maybe Int
eval (Lit x) = Just x
eval (Add x1 x2) = (justAdd (eval x1) (eval x2))
eval (Sub x1 x2) = (justSub (eval x1) (eval x2))
eval (Prod x1 x2) = (justProd (eval x1) (eval x2))
eval (Div x1 x2) = (justDiv (eval x1) (eval x2))
