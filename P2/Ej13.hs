divisors y = [x | x <- [1..y], mod y x == 0]
matches y list = [x | x <- list, x == y]
cuadruplas n = [(a,b,c,d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], (a^2 + b^2) == (c^2 + d^2), a/=c, a/=d, b/=c, b/=d]
unique [] = []
unique (x:xs) = x : [y | y <- unique xs, y /= x]

scalarProduct xs ys = [fst y + snd y | y <- (zip xs ys)]