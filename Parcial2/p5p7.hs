module E5 where
import qualified ListSeq
import Seq 
import Par

-- ITEM A

max3 r r' xs | length r >= length r' && length r >= length xs = r
             | length r' >= length r && length r' >= length xs = r'
             | otherwise = xs

max2 r r' | length r >= length r' = r
          | otherwise = r'

_sccml :: [Int] -> (Int, [Int], [Int], [Int], Int, Int)
_sccml [x] = (x, [x], [x], [x], x, 1)
_sccml xs = let ((i1, p1, r1, s1, e1, l1), (i2, p2, r2, s2, e2, l2)) = _sccml (take (div (length xs) 2) xs) ||| _sccml (drop (div (length xs) 2) xs)
                i = i1
                p = if ((length p1) == l1 && e1 < i2) then p1 ++ p2
                                                      else p1
                r = if (e1 < i2) then max3 r1 r2 (s1 ++ p2)
                                 else max2 r1 r2
                s = if ((length s2) == l2 && e1 < i2) then s1 ++ s2
                                                      else s2
                e = e2
                l = l1 + l2
            in (i, p, r, s, e, l)           

sccml :: [Int] -> Int
sccml xs = let (i, p, r, s, e, l) = _sccml xs
           in (length r) - 1

-- ITEM B

combine :: (Int, [Int], [Int], [Int], Int, Int) -> (Int, [Int], [Int], [Int], Int, Int) -> (Int, [Int], [Int], [Int], Int, Int)
combine (i1, p1, r1, s1, e1, l1) (i2, p2, r2, s2, e2, l2) = let i = i1
                                                                p = if ((length p1) == l1 && e1 < i2) then p1 ++ p2
                                                                                                    else p1
                                                                r = if (e1 < i2) then max3 r1 r2 (s1 ++ p2)
                                                                                else max2 r1 r2
                                                                s = if ((length s2) == l2 && e1 < i2) then s1 ++ s2
                                                                                                    else s2
                                                                e = e2
                                                                l = l1 + l2
                                                            in (i, p, r, s, e, l)     

base :: Int -> (Int, [Int], [Int], [Int], Int, Int)
base x = (x, [x], [x], [x], x, 1)

_sccml' :: [Int] -> (Int, [Int], [Int], [Int], Int, Int)
_sccml' xs = reduceS combine (0, [], [], [], 0, 0) (mapS base xs) 

sccml' :: [Int] -> Int
sccml' xs = let (i, p, r, s, e, l) = _sccml' xs
            in (lengthS r) - 1