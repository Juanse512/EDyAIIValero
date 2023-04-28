data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show


maximumT :: Ord a => Bin a -> a
maximumT (Nodo _ a Hoja) = a
maximumT (Nodo l a r) = maximumT r

minimumT :: Ord a => Bin a -> a
minimumT (Nodo Hoja a r ) = a
minimumT (Nodo l a r ) = minimumT l

checkBST :: Ord a => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja v Hoja) = True
checkBST (Nodo l v Hoja) = if v >= (maximumT l) then (checkBST l) else False 
checkBST (Nodo Hoja v r) = if v <= (minimumT r) then (checkBST r) else False 
checkBST (Nodo l v r) = if v <= (minimumT r) && v >= (maximumT l) then (checkBST l) && (checkBST r) else False

splitBST :: Ord a => Bin a -> a -> (Bin a, Bin a)
splitBST Hoja _ = (Hoja, Hoja)
splitBST (Nodo l v r) x = let (ll, lr) = splitBST l x
                              (rl, rr) = splitBST r x
                              in if x > v then ((Nodo l v rl), rr) else (ll, (Nodo lr v r))
                                
insert :: Ord a => a -> Bin a -> Bin a
insert a Hoja = Nodo Hoja a Hoja
insert a (Nodo l b r ) = if a <= b then Nodo (insert a l) b r
                           else Nodo l b (insert a r )

joinBST :: Ord a => Bin a -> Bin a -> Bin a
joinBST Hoja v2 = v2
joinBST v1 Hoja = v1
joinBST (Nodo l1 v1 r1) (Nodo l2 v2 r2) = joinBST r1 (joinBST l1 (insert v1 (Nodo l2 v2 r2)))