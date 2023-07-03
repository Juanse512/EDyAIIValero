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

newtype Punto2d = P2d (Double, Double) deriving Show
newtype Punto3d = P3d (Double, Double, Double) deriving Show

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


type Rect = (Punto2d, Punto2d)
-- Revisamos si un punto esta dentro de un rectangulo
inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (a,b)) (P2d (x1,y1), P2d (x2,y2)) = if ((a <= x1 && a >= x2) || (a >= x1 && a <= x2)) &&
                                    ((b <= y1 && b >= y2) || (b >= y1 && b <= y2)) then True else False
-- Busqueda ortogonal optimizada, si estamos a la izquierda o por debajo del rectangulo (dependiendo del eje de la hoja)
-- , nos movemos hacia la derecha, si no a la izquierda
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node l v@(P2d (a, b)) r k) rect@(P2d (x1,y1), P2d (x2,y2)) = let res = inRegion v rect in 
                                                                           if res == True then (v:((ortogonalSearch l rect)++(ortogonalSearch r rect)))
                                                                           else
                                                                                if k == 0 then 
                                                                                    if a <= (min x1 x2) then (ortogonalSearch r rect)
                                                                                                        else (ortogonalSearch l rect)
                                                                                else
                                                                                    if b <= (min y1 y2) then (ortogonalSearch r rect)
                                                                                                        else (ortogonalSearch l rect)
                                                                                
                                                                                    