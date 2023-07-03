import Par
data BTree a = E | L a | N Int (BTree a) (BTree a) deriving Show

size :: BTree a -> Int
size E = 0
size (L a) = 1
size (N s l r) = s

mapIndexAux :: (a -> Int -> b) -> Int  -> BTree a -> BTree b
mapIndexAux f ind E = E
mapIndexAux f ind (L x) = L (f x ind)
mapIndexAux f ind (N s l r) = let (ll, rr) = mapIndexAux f ind l ||| mapIndexAux f (ind + (size l)) r in N s ll rr

mapIndex :: (a -> Int -> b) -> BTree a -> BTree b
mapIndex f t = mapIndexAux f 0 t

createTree :: Int -> Int -> Int -> BTree Int
createTree st end n | st == end = (L n)
                    | end - st == 1 = N 2 (L n) (L n)
                    | otherwise = let m = (div (st+end) 2)
                                      (ll,rr) = createTree st m n ||| createTree (m+1) end n in (N ((size ll) + (size rr)) ll rr)

miF :: Int -> Int -> Int -> Int
miF 0 t v = t
miF k t v = t + (div v k)

fromSlow :: Int -> Int -> Int -> BTree Int
fromSlow n m k = mapIndex (miF k) (createTree 0 (m-1) n)