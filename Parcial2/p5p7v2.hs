

combine :: (Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int)
combine (st1, end1, p1, s1, t1, l1) (st2, end2, p2, s2, t2, l2) = let st = st1
                                                                      end = end2
                                                                      p = if p1 == l1 && end1 < st2 then p1+p2 else p1
                                                                      s = if s2 == l2 && end1 < st2 then s1+s2 else s2
                                                                      t = if end1 < st2 then max (max t1 t2) s1+p2 else
                                                                           max t1 t2
                                                                      l = l1 + l2 in (st,end,p,s,t,l)

scmml :: [Int] -> (Int,Int,Int,Int,Int,Int)
scmml [] = (0,0,0,0,0,0)
scmml [x] = (x,x,1,1,1,1)
sccml xs = let (a,b) = (sccml (take (div (length xs) 2) xs), sccml (drop (div (length xs) 2) xs))
                    in combine a b
