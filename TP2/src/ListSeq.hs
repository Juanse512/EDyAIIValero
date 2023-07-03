module ListSeq where
import Par
import Seq

--Dada una funcion y un n devuelve una lista con la funcion aplicada a los primeros n elementos
tabulateList :: (Int -> a) -> Int -> Int -> [a]
tabulateList f n i  | (n == i) = []
                    | (n /= i) = let (y, ys) = ((f i) ||| (tabulateList f n (i+1))) in (y:ys)

-- Aplica una funcion a todos los elementos de la lista
mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = let (y, ys) = ((f x) ||| (mapList f xs)) in (y:ys)

-- Dada una funcion booleana y una secuencia devuelve la subsecuencia mas larga donde se cumple la funcion
filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (x:xs) = let (y, ys) = ((f x) ||| (filterList f xs)) in if (y) then (x:ys) else (ys)

-- Devuelve una secuencia como Treeview, mitad del array va al nodo izquierdo y la otra mitad al derecho
showtList :: [a] -> Int -> TreeView a [a]
showtList [] len  = EMPTY
showtList [x] len = ELT x
showtList xs len  = NODE (take (floor (fromIntegral (len)/2)) xs) (drop ( floor (fromIntegral(len)/2)) xs) 

-- Muestra una secuencia como ListView
showlList :: [a] -> ListView a [a]
showlList [] = NIL
showlList (x:xs) = CONS x xs

-- Dada una secuencia de secuencias las concatena creando una sola lista
joinList :: [[a]] -> [a]
joinList [] = []
joinList [a] = a
joinList (x:xs:xss) = let (x1, x2) = (appendS x xs) ||| (joinList xss) in appendS x1 x2


-- Reduce una lista a un elemento aplicando una funcion a todos los elementos
reduceList :: (a -> a -> a) -> [a] -> a
reduceList f [x] = x
reduceList f (x:xs:[]) = (f x xs)  
reduceList f (x:xs:xss) = let (a,b) = (f x xs) ||| (reduceList f xss) in f a b
-- Funcion auxiliar del reduce, le aplica el elemento neutro al resultado de reduceList
reduceListPT :: (a -> a -> a) -> a -> [a] -> a
reduceListPT f e [] = e
reduceListPT f e xs = f e (reduceList f xs)

-- Funcion contraer, toma una lista y una funcion, aplica la funcion de a pares de la lista y los retorna
-- en una nueva lista contrayendola a la mitad.
contraer :: (a -> a -> a) -> [a] -> [a]
contraer f [] = []
contraer f [x] = [x]
contraer f (x:xs:xss) = let (a,b) = (f x xs) ||| (contraer f xss) in a:b

-- Funcion expandir, toma la lista original y un output parcial y genera el resultado deseado en ese nivel
-- usando una logica de expansion
expandir :: (a -> a -> a) -> [a] -> [a] -> [a]
expandir f [] [] = []
expandir f [] [p] = [p]
expandir f [s] [p] = [p]
expandir f (s:ss:sss) (p:ps) = let (a,b) = (f p s) ||| (expandir f sss ps) in (p:a:b)

-- Funcion de scaneo, retorna la reduccion de la list y los resultados intermedios acumulativamente en una lista
scanL :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanL f b [] = ([], b) 
scanL f b [x] = ([b], f b x) 
scanL f b xs = let (p, result) = scanL f b (contraer f xs) in ((expandir f xs p), result)

instance Seq [] where
    emptyS = []
    singletonS x = [x]
    lengthS xs = length xs
    nthS xs n = xs!!n 
    tabulateS f n = tabulateList f n 0
    mapS f xs = mapList f xs
    filterS f xs = filterList f xs
    appendS xs ys = (xs++ys)
    takeS xs n = (take n xs)
    dropS xs n = (drop n xs)
    showtS xs = showtList xs (length xs)
    showlS xs = showlList xs
    joinS xs = joinList xs
    reduceS f e xs = reduceListPT f e xs
    scanS f e xs = scanL f e xs
    fromList xs = xs
