import Data.List
data NdTree p = Node (NdTree p) -- sub ́arbol izquierdo
            p -- punto
            (NdTree p) -- sub ́arbol derecho
            Int | Empty deriving (Eq, Ord, Show)

class Punto p where
            dimension :: p -> Int -- devuelve el n ́umero de coordenadas de un punto
            coord :: Int -> p -> Double -- devuelve la coordenada k- ́esima de un punto (comenzando de 0)
            dist :: p -> p -> Double -- calcula la distancia entre dos puntos



-- Toma dos puntos de grado K y calcula la distancia entre ellos
distK :: Punto p => p -> p -> Int -> Double
distK p1 p2 0 = ((coord 0 p2) - (coord 0 p1))^2
distK p1 p2 k = (((coord k p2) - (coord k p1))^2) + (distK p1 p2 (k-1))

newtype Punto2d = P2d (Double, Double) deriving (Eq, Show)
newtype Punto3d = P3d (Double, Double, Double) deriving (Eq, Show)

instance Punto Punto2d where
    dimension p1 = 2
    coord 0 (P2d (x1,x2)) = x1
    coord 1 (P2d (x1,x2)) = x2
    dist p1 p2 = distK p1 p2 1

instance Punto Punto3d where
    dimension p1 = 3
    coord 0 (P3d (x1,x2,x3)) = x1
    coord 1 (P3d (x1,x2,x3)) = x2
    coord 2 (P3d (x1,x2,x3)) = x3
    dist p1 p2 = distK p1 p2 2

{- 
    Toma una lista, un indice inicial y una coordenada, itera sobre 
    la lista hasta que encuentre un punto con un valor distinto al inicial
    en dicha coordenada.
-}
lastEquivalent :: Punto p => [p] -> Int -> Int -> Int
lastEquivalent [x] v d = v
lastEquivalent x v d = if (v+1) >= (length x) then v else if coord d (x!!v) == coord d (x!!(v+1)) then lastEquivalent x (v+1) d else v

-- Calcula la mediana de la lista
medianNum :: Punto p => [p] -> Int -> Int
medianNum l k = lastEquivalent l (div (length l) 2) k

-- Compara la k-esima coordenada de dos puntos
compareCoordinates :: Punto p => Int -> p -> p -> Ordering
compareCoordinates k x y  = (compare (coord k x) (coord k y))

{-  
    Funcion auxiliar para formar el arbol, toma ademas de la lista el nivel
    sobre el que se esta formando el arbol en ese nodo y recursa sobre este
    para formar los subarboles. 
-}
fromListAux :: Punto p => [p] -> Int -> NdTree p
fromListAux [] _ = Empty
fromListAux l k1 =  let
                        k = (mod k1 (dimension (head l)))       -- Coordenada del nodo
                        ls = (sortBy (compareCoordinates k) l)  -- Lista ordenada segun la coordenada que se trabaja
                        m = (medianNum ls k) in                 -- Indice del punto a usar como raiz
                        (Node (fromListAux (take m ls) (k1 + 1)) (ls!!m) (fromListAux (drop (m+1) ls) (k1 + 1)) k) 

-- Dada una lista de puntos forma el arbol binario
fromList :: Punto p => [p] -> NdTree p
fromList l = fromListAux l 0