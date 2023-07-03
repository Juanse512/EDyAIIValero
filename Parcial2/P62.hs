data Tree a = E | Leaf a | Join (Tree a) (Tree a)





mcss :: (Num a, Ord a) => Tree a -> (a, a, a, a)
mcss (Leaf x) = ((max x 0), (max x 0), (max x 0), x)
mcss (Join l r) = let   ((m1,p1,s1,t1), (m2,p2,s2,t2)) = (mcss l, mcss r)
                        m = max (max m1 m2) (s1 + p2)
                        p = max p1 (t1 + p2)
                        s = max s2 (t2 + s1)
                        t = t1 + t2
                        in (m,p,s,t)
                                                                     
