
import Seq
import ListSeq
data Tree key val = E | J Int (Tree key val) (key,val) (Tree key val) deriving Show
data Result = All Int | Some (Int, Int, Int) deriving Show
size :: Tree a b -> Int
size E = 0
size (J v l x r) = v 

merge :: Ord a => Tree a b -> Tree a b -> Tree a b
merge E t = t
merge t E = t
merge h1@(J x l (k,v) r) h2@(J x' l' (k', v') r') | k > k' = let mg = (merge h1 r') in (J ((size l') + (size mg) + 1) l' (k',v') mg)
                                                  | otherwise = let mg = (merge h2 r) in (J ((size l) + (size mg) + 1) l (k,v) mg)

splitMax :: Ord a => Tree a b -> ((a,b), Tree a b)
splitMax (J k l kv E) = (kv, l)
splitMax (J k h1 kv h2) = let (rv, rt) = (splitMax h2) in (rv, (J (k - (size h2) + (size rt)) h1 kv rt))

delete :: Ord a => a -> Tree a b -> Tree a b
delete x E = E
delete x (J s l (k, v) r) | x == k = merge l r
                          | x < k = let res = delete x l in J (s - (size l) + (size res)) res (k,v) r
                          | x > k = let res = delete x r in J (s - (size r) + (size res)) r (k,v) res

mapF :: Int -> (Int,Int,Int,Int,Int,Int)
mapF x = (x,x,x,x,0,1)

combine :: (Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int)
combine (st1, e1, p1, s1, m1, t1) (st2, e2, p2, s2, m2, t2) = let   st = st1
                                                                    e = e2
                                                                    p = if st1 == 0 then 0 else if p1 == t1 then p1+p2 else p1
                                                                    s = if e2 == 0 then 0 else if s2 == t2 then s1+s2 else s2
                                                                    mm = max m1 m2
                                                                    m = if st2 == 0 && s1 /= t1 && e1 == 0 && p2 /= t2 then s1 + p2 else
                                                                        if st2 == 0 && s1 /= t1 then s1 else if e1 == 0 && p2 /= t2 then p2 else max m1 m2
                                                                    t = t1+t2
                                                                in (st, e, p, s, (max m mm), t)

exc :: [Int] -> (Int,Int,Int,Int,Int,Int)
exc xs = reduceS combine (1,1,0,0,0,0) (mapS mapF xs)           


mapR :: Int -> Result
mapR 1 = All 1
mapR 0 = Some (0,0,0)

combineRes :: Result -> Result -> Result
combineRes (All n1) (All n2) = All (n1+n2)
combineRes (All n) (Some (i,j,m)) = Some ((i+n),j,m)
combineRes (Some (i,j,m)) (All n) = Some (i,(j+n),m)
combineRes (Some (i,j,m)) (Some (i',j',m')) = Some (i,j',(j+i'))

exclamation :: [Int] -> Result
exclamation xs = reduceS combineRes (All 0) (mapS mapR xs)