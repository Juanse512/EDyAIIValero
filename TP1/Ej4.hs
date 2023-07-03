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

-- Revisa si el arbol esta vacio
isEmpty :: NdTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Sirve para volver a poner los ejes de los nodos en el numero correcto si hay un cambio, en concreto, cuando
-- eliminamos un elemento, el subarbol hijo tiene el eje en una unidad mas de lo necesario
reorder ::  (Eq p, Punto p) => NdTree p -> NdTree p
reorder Empty = Empty
reorder (Node l v r k) = (Node (reorder l) v (reorder r) (mod (k-1) (dimension v)))
-- Busca el mayor elemento del subarbol izquierdo para utilizarlo como reemplazo al eliminar un nodo, devuelve el valor y el subarbol 
-- con los ejes re-ordenados
searchSuccessor :: (Eq p, Punto p) => NdTree p -> (NdTree p, p)
searchSuccessor (Node l v Empty k) = (reorder(l), v)
searchSuccessor (Node l v r k) = let (l1,v1) = searchSuccessor(r) in
                                ((Node l v l1 k), v1)
-- Elimina la raiz del arbol que le estamos pasando y la reemplaza por el sucesor encontrado en la funcion searchSuccessor
deleteRoot :: (Eq p, Punto p) => NdTree p -> NdTree p
deleteRoot Empty = Empty
deleteRoot h1@(Node Empty v r k) = reorder(r) -- Si no tenemos subarbol izquierdo, solamente reordenamos el derecho
deleteRoot h1@(Node l v r k) = let (l1,v1) = searchSuccessor(l) in (Node l1 v1 r k) -- Si tiene subarbol izquierdo, buscamos el mayor elemento de este y lo ponemos en el nodo actual

-- Dado un punto, lo busca en el arbol y lo elimina
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar _ Empty = Empty
eliminar x h1@(Node l v r k) = if x == v then (deleteRoot h1) else
                               if (coord k x) <= (coord k v) then
                                    (Node (eliminar x l) v r k)
                                    else (Node l v (eliminar x r) k)
