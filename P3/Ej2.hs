data Linea = L [Char] Int deriving Show

vacia :: Linea
vacia = L [] 0

moverIzq :: Linea -> Linea
moverIzq (L c 0) = L c 0
moverIzq (L c p) = L c (p - 1)

moverDer :: Linea -> Linea
moverDer (L c p) = if p == ((length c) - 1) then L c p else L c (p + 1)

moverIni :: Linea -> Linea
moverIni (L c p) = L c 0

moverFin :: Linea -> Linea
moverFin (L c p) = L c ((length c) - 1)

insertarA :: Char -> Linea -> [Char]
insertarA ch (L c 0) = ch : c
insertarA ch (L (c:cs) p) = c : insertarA ch (L cs (p-1)) 

insertar :: Char -> Linea -> Linea
insertar ch (L c p) = L (insertarA ch (L c p)) p

borrarA :: Linea -> [Char]
borrarA (L (c:cs) 0) = cs
borrarA (L (c:cs) p) = c : borrarA (L cs (p-1)) 

borrar :: Linea -> Linea
borrar (L c p) = if p == ((length c) - 1) then L (borrarA (L c p)) (p-1) else L (borrarA (L c p)) p
-- [1 2 3] 1 : [2 3] 1 : 2 : [3]