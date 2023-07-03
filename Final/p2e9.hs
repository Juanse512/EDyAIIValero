zip3R :: [a] -> [a] -> [a] -> [(a,a,a)]
zip3R [] b c = []
zip3R a [] c = []
zip3R a b [] = []
zip3R (a:as) (b:bs) (c:cs) = (a,b,c):(zip3R as bs cs)

merge3 :: [a] -> [(a,a)] -> [(a,a,a)]
merge3 [] b = []
merge3 a [] = []
merge3 (a:as) (b:bs) = (a, fst b, snd b):(merge3 as bs)

zip3N :: [a] -> [a] -> [a] -> [(a,a,a)]
zip3N a b c = let n = zip a b in merge3 c n

type NumBin = [Bool]

binSumAux :: Bool -> Bool -> Bool -> (Bool,Bool)
binSumAux a b c = if c == True then if a == True && b == True then (True, True) else if b == True || a == True then (False,True) else (True,False)
                  else if a == True && b == True then (False, True) else if b == True || a == True then (True,False) else (False,False)

sumBin :: NumBin -> NumBin -> Bool -> NumBin
sumBin [] [] b = [b]
sumBin (a:as) [] b = let (n,c) = binSumAux a False b in n:(sumBin as [] c)
sumBin [] (b:bs) x = let (n,c) = binSumAux b False x in n:(sumBin [] bs c)
sumBin (a:as) (b:bs) x = let (n,c) = binSumAux a b x in n:(sumBin as bs c)

divisors :: Int -> [Int]
divisors v = [x | x <- [1..v], (mod v x == 0)]

matches :: Int -> [Int] -> [Int]
matches y ys = [x | x <- ys, (x == y)]

cuadruple :: Int -> [(Int, Int, Int, Int)]
cuadruple n = [(a,b,c,d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], (a /= b), (a /= c), (a /= d), (b /= c), (b /= d), (d /= c),
                a^2 + b^2 == c^2 + d^2]