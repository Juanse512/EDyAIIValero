data T a = E | N (T a) a (T a) deriving Show

combinar :: T a -> T a -> T a
combinar E t2 = t2
combinar t1 E = t1
combinar (N E t1 r1) t2 = N t2 t1 r1
combinar (N l1 t1 E) t2 = N l1 t1 t2
combinar (N l1 t1 r1) (N l2 t2 r2) = let (ll,rr) = ((combinar l1 (N l2 t2 E)), (combinar r1 r2)) in (N ll t1 rr)



filterT :: (a -> Bool) -> T a -> T a
filterT f E = E
filterT f (N l v r) = let (ll, rr) = (filterT f l, filterT f r) in if f v then (N ll v rr) else (combinar rr ll)



quickSortT :: Ord a => T a -> T a
quickSortT E = E
quickSortT (N l v r) = let (ll, rr) = ((combinar (filterT (\x -> x < v) r) (filterT (\x -> x < v) l)), (combinar (filterT (\x -> x >= v) r) (filterT (\x -> x >= v) l)))
                            in (N ll v rr)
