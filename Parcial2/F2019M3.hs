import Par
import Seq
import ListSeq
data Tree a = E | L a | N Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size E = 0
size (L a) = 1
size (N s l r) = s

trimR :: Int -> Tree a -> Tree a
trimR v E = E
trimR v (L x) = (L x)
trimR v (N s l r) | v == (size l) = l 
                  | v < (size l) = (trimR v l) 
                  | otherwise = let res = (trimR (v-(size l)) r) in (N (s - (size r) + (size res)) l res)

trimL :: Int -> Tree a -> Tree a
trimL v E = E
trimL v (L x) = (L x)
trimL v (N s l r) | v == (size l) = r 
                  | v < (size l) = let res = (trimL v l) in (N (s - (size l) + (size res)) res r) 
                  | otherwise = (trimL (v-(size l)) r)


compareTree :: Eq a => Tree a -> Tree a -> Bool
compareTree (L a) (L b) = if a == b then True else False
compareTree (L a) _ = False
compareTree _ (L b) = False
compareTree (N s1 l1 r1) (N s2 l2 r2) | s1 == s2 = let (a,b) = compareTree l1 l2 ||| compareTree r1 r2 in a && b
                                      | otherwise = False

stripSuffix :: Eq a => Tree a -> Tree a -> Tree a
stripSuffix E t = t
stripSuffix t1@(N ss ls rs) t2@(N s l r) | ss == s = if compareTree t1 t2 then E else t2
                                         | ss > s = t2
                                         | ss < s = let (ll, rr) = stripSuffix (trimR (ss - (size r - ss)) t1) t2 ||| stripSuffix (trimL (size r - ss) t1) t2
                                                        in (N ((size ll) + (size rr)) ll rr)

mapF :: Int -> (Int, Int, Int, Int)
mapF x = ((max x 0), (max x 0), (max x 0), x)

combine :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
combine (m,p,s,t) (m',p',s',t') = let mf = max (max m m') (s+p')
                                      pf = max p (t+p')
                                      sf = max s' (t'+s)
                                      tf = t + t'
                                      in (mf,pf,sf,tf)
maxBalance :: [Int] -> Int
maxBalance xs = let (m,p,s,t) = reduceS combine (0,0,0,0) (mapS mapF xs) in m