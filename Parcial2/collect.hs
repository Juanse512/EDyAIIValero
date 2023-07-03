sortKeys :: (Ord a) => [(a, b)] -> [(a, b)]
sortKeys xs = reduceS (merge f) emptyS (mapS (\i -> (singletonS i)) xs) 
    where f (a, x) (b, y) | a > b = GT
                          | a < b = LT
                          | otherwise = EQ

seqkeys :: (a, b) -> [(a, [b])]
seqkeys (a, b) = [(a, (singletonS b))]

groupKeys :: (Eq a) => [[(a, [b])]] -> [(a, [b])]
groupKeys xs = reduceS f emptyS xs 
    where f [] bs = bs
          f as bs = let (a, b) = nthS as ((lengthS as) - 1)
                        (x, y) = nthS bs 0
                    in if a == x then let zs = (appendS (singletonS (x, appendS y b)) (dropS bs 1))
                                          ws = (takeS as ((lengthS as) - 1)) 
                                      in appendS zs ws
                                 else appendS as bs

collect :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
collect xs = groupKeys (map seqkeys (sortKeys xs))