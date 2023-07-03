-- 1) a)
-- empty = empty

-- insert k v vacio = insert k v vacio
-- insert k v (insert k' v' m) = if k == k' then (insert k v m) else insert k v (insert k' v' m)

-- erase k vacio = vacio
-- erase k (insert k' v  m) if k == k' then m else insert k' v (erase k m)

-- isKey k vacio = False
-- isKey k (insert k' v  m) = if k == k' then true else isKey k m

-- lookup k vacio = Nothing
-- lookup k (insert k' v  m) = if k == k' then Just v else lookup k m

-- b)  lookup k (insert k v t) = Just v 
-- Pruebo por inducción estructural sobre BST
-- caso base E
    -- <insert 1>
        -- N E (k,v) E
    -- <lookup 2 (1)>
        -- tengo k == k, por (1) Just v

-- Caso inductivo, supongo lookup k (insert k v l) == Just v,  lookup k (insert k v r) == Just v (HI), pruebo para N l (q,w) r
    -- <insert 2> insert k v (N l (q,w) r)
        -- Supongo k == q 
        --         <insert2.1> N l (k,v) r
        --         <lookup 2.1>
        --         k == q por hipotesis*, Just v
        -- Supongo k < q
        --         <insert.2.2> N (insert k v l) (q,w) r
        --         <lookup 2.2>
        --         k < q por hipotesis, lookup k (insert k v l)
        --         (HI)
        --         Just v
        -- Supongo k > q
        --         <insert.2.3>N l (q,w) (insert k v r)
        --         <lookup 2>
        --         k > q por hipotesis, lookup k (insert k v r)
        --         (HI)
        --         Just v, probado

import Seq
import ListSeq
import Par

data Tree a = E | N Int (Tree a) a (Tree a) deriving Show

size :: Tree a -> Int
size E = 0
size (N v _ _ _) = v

filterPrefix :: (a -> Bool) -> Tree a -> Tree a
filterPrefix f E = E
filterPrefix f (N s l x r) = let (ll,rr) = (filterPrefix f l) ||| (filterPrefix f r) in
                                if (size l) /= (size ll) then ll
                                else if (f x) then (N (s - (size r) + (size rr)) ll x rr) else ll


mapF :: Float -> Float -> (Int, Int)
mapF x v = if v >= x then (1,1) else (0,0)

combine :: (Int, Int) -> (Int, Int) -> (Int, Int)
combine (v,s) (v',s') = if v' == 1 then (v, s+s') else (v',s')

maxT :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxT (v, s) (v',s') = if s > s' then (v,s) else (v',s')

longestStreak :: Float -> [Float] -> Int 
longestStreak x xs = let (a,(b1,b2)) = scanS combine (0,0) (mapS (mapF x) xs)
                         (v, res) = reduceS maxT (0,0) a 
                        in max res b2

longestStreakSS :: Float -> [Float] -> [(Int, Int)] 
longestStreakSS x xs = let (a,b) = scanS combine (0,0) (mapS (mapF x) xs)
                        in a