zipN [] _ _ = []
zipN _ [] _ = []
zipN _ _ [] = []
zipN (x:xs) (y:ys) (z:zs) = (x,y,z) : zipN xs ys zs

mergeD [] _ = []
mergeD _ [] = []
mergeD (x:xs) (y:ys) = (fst x, snd x, y) : mergeD xs ys

zipN2 x y z = let n = zip x y in mergeD n z 