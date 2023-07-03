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

-- Inserta un punto a la izquierda de un nodo
insertarIzq :: Punto p => p -> NdTree p -> NdTree p
insertarIzq x (Node _ y r n) = (Node (Node Empty x Empty (mod (n+1) (dimension x))) y r n)

-- Inserta un punto a la derecha de un nodo 
insertarDer :: Punto p => p -> NdTree p -> NdTree p
insertarDer x (Node l y _ n)  = (Node l y (Node Empty x Empty (mod (n+1) (dimension x))) n)

-- Devuelve true si el arbol esta vacio false si no
isEmpty :: NdTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

{- 
    Recorre el arbol recursivamente buscando el lugar donde debe ir el punto
    en su ultima recursion lo inserta a izquierda o derecha del nodo padre
    que le corresponda.
-}
insertarAux :: Punto p => p -> NdTree p -> NdTree p
insertarAux x Empty = Empty
insertarAux x t@(Node l y r n) = let res = if (coord n x) <= (coord n y) then insertarAux x l else insertarAux x r
                            in 
                                if (isEmpty res) then
                                    if (coord n x) <= (coord n y) 
                                        then insertarIzq x t
                                        else insertarDer x t
                                else 
                                    if (coord n x) <= (coord n y) 
                                        then (Node res y r n) 
                                        else (Node l y res n)

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar x Empty = (Node Empty x Empty 0)
insertar x t = insertarAux x t