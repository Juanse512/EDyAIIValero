

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) = if x <= y then x : (merge xs (y:ys))