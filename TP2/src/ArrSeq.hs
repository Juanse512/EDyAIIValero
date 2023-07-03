module ArrSeq where
import qualified Arr as A
import Seq
import Par
-- Dado un elemento, devuelve un array con ese único elemento
singletonArr :: a -> A.Arr a
singletonArr x = fromList [x]

-- Dada una funcion que booleana y un elemento, si la funcion evaluada en ese elemento es true
-- devuelve un singleton con ese valor, si no devuelve vacio
filterFunction :: (a -> Bool) -> a -> A.Arr a
filterFunction f e = if f e then singletonS e else emptyS

-- Dado una funcion booleana, un indice y un array devuelve la funcion evaluada en el elemento que esta en el indice
-- del array y el elemento siguiente
contraerAux :: (a -> a -> a) -> Int -> A.Arr a -> a
contraerAux f i xs | i == (lengthS xs - 1) = (nthS xs i)
                   | otherwise = (f (nthS xs i) (nthS xs (i + 1)))

-- Dada una funcion y un array, aplica una contraccion del array con la funcion
contraer :: (a -> a -> a) -> A.Arr a -> A.Arr a
contraer f xs = tabulateS (\x -> contraerAux f (2*x) xs) (ceiling(fromIntegral (lengthS xs) / 2))

-- Aplica el paso de expansion del scan dado dos arrays y un indice, si es par devuelve el i-esimo elemento
-- de p y si es impar le aplica la funcion dada al (i-1)-esimo elemento de s y el (i/2)-esimo elemento de p
expandirAux :: (a -> a -> a) -> A.Arr a -> A.Arr a -> Int -> a
expandirAux f s p i | (even i) = nthS p (i `div` 2)
                    | otherwise = f (nthS s (i-1)) (nthS p (i `div` 2))

-- Aplica la funcion expandir a dos arrays dados con tabulate
expandir :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expandir f s p = tabulateS (\x -> expandirAux f s p x) (lengthS s)

-- Reduce el array dado a un elemento usando una funcion aplicada a todos los elementos del array
reduceArr :: (a -> a -> a) -> a -> A.Arr a -> a
reduceArr f b xs | (lengthS xs) == 0 = b 
                 | (lengthS xs) == 1 = f b (nthS xs 0) 
                 | otherwise = let ys = (contraer f xs) in reduceArr f b ys

-- Funcion de scaneo, retorna la reduccion del array y los resultados intermedios acumulativamente en un array
scanArr :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanArr f b xs | (lengthS xs) == 0 = (emptyS, b)
               | (lengthS xs) == 1 = (singletonS b, f b (nthS xs 0)) 
               | otherwise = let (p, result) = scanArr f b (contraer f xs) in ((expandir f xs p), result)

-- Funcion para visualizar un array como TreeView
showtArr :: A.Arr a -> Int -> TreeView a (A.Arr a)
showtArr xs 0  = EMPTY
showtArr xs 1 = ELT (nthS xs 0)
showtArr xs len = NODE (takeS xs (floor (fromIntegral (len)/2)) ) (dropS xs ( floor (fromIntegral(len)/2))) 

-- Funcion para visualizar un array como ListView
showlArr :: A.Arr a -> ListView a (A.Arr a)
showlArr xs | lengthS xs > 0 = CONS (nthS xs 0) (dropS xs 1)
            | otherwise = NIL

instance Seq A.Arr where
    emptyS = A.empty
    singletonS x = singletonArr x
    lengthS xs = A.length xs
    nthS xs n = xs A.! n 
    tabulateS f n = (A.tabulate f n)
    takeS xs n = (A.subArray 0 n xs)
    dropS xs n = (A.subArray n (A.length xs - n) xs)
    mapS f xs = tabulateS (\x -> (f (xs A.! x))) (lengthS xs)
    filterS f xs = joinS (tabulateS (\x -> (filterFunction f (xs A.! x))) (lengthS xs))
    appendS xs ys = A.flatten (A.fromList [xs,ys])
    fromList xs = A.fromList xs
    joinS xs = A.flatten xs
    reduceS f e xs = reduceArr f e xs
    showtS xs = showtArr xs (A.length xs)
    showlS xs = showlArr xs
    scanS f e xs = scanArr f e xs

