data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving Show


memberAux :: Int -> Int -> Tree Int -> Bool
memberAux c x Hoja = c == x
memberAux c x (Nodo l v r) = if x <= v then memberAux v x l else memberAux c x r

