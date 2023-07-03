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

newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

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