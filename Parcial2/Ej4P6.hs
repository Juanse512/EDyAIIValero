data T a = E | N (T a) a (T a) deriving Show

combinar :: T a -> T a -> T a
combinar E E = E
combinar E t2 = t2
combinar t1 E = t1
combinar (N l v r) t2 = (N (combinar l r) v t2)


isEmpty :: T a -> Bool
isEmpty E = True
isEmpty _ = False

filterT :: (a -> Bool) -> T a -> T a
filterT f E = E
filterT f (N E v E) = if f v then (N E v E) else E
filterT f (N l v r) = let (ll, rr) = (filterT f l, filterT f r)
                      in if f v then (N ll v rr) 
                      else (combinar ll rr)


data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

getSize :: BTree a -> Int
getSize Empty = 0
getSize (Node s l v r) = s

splitAtT :: BTree a -> Int -> (BTree a, BTree a)
splitAtT Empty i = (Empty, Empty)
splitAtT (Node s l v r) i | (s - (getSize l)) == i = ((Node (s - (getSize r)) l v Empty), r)
                          | i == (getSize l) = (l, (Node (s - getSize l) Empty v r))
                          | i > (s - (getSize l)) = let (t1, t2) = splitAtT r (i - ((s - getSize l)))
                                                    in ((Node ((s - getSize r) + (getSize t1)) l v t1),t2)
                          | otherwise = let (t1, t2) = splitAtT l i
                                        in (t1, (Node ((s - getSize l) + (getSize t2)) t2 v r))
    
                        --      if (s - (getSize l)) == i then ((Node (s - (getSize r)) l v Empty), r)
                        --    else if i == (getSize l) then (l, (Node (s - getSize l) Empty v r))
                        --    else if i > (s - (getSize l)) then splitAtT r (i - ((s - getSize l))) else splitAtT l i