data Tree a = E | Leaf a | Join (Tree a) (Tree a)





tNode :: (Num a, Ord a) => a -> (a,a,a,a)
tNode x = ((max x  0), (max x 0), (max x 0), x)

combine :: (Num a, Ord a) =>  (a,a,a,a) -> (a,a,a,a) -> (a,a,a,a)
combine (p1,s1,m1,t1) (p2,s2,m2,t2) = let p = if p1 == t1 then p1+p2 else p1
                                          s = if s2 == t2 then s1+s2 else s2
                                          m = max (max m1 m2) (s1 + p2)
                                          t = (t1 + t2)
                                      in (p,s,m,t) 

mcss :: (Num a, Ord a) => Tree a -> (a,a,a,a)
mcss (Leaf x) = tNode x
mcss (Join l r) = let (ll, rr) = (mcss l, mcss r) in combine ll rr