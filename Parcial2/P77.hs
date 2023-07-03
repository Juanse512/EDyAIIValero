
import Seq
import ListSeq




mapF :: Int -> [Int]
mapF x = [x]

headS :: [a] -> a
headS xs = let (CONS x t) = showlS xs in x

tailS :: [a] -> [a]
tailS xs = let (CONS x t) = showlS xs in t

lastS :: [a] -> a
lastS xs = nthS xs (lengthS xs - 1)

combine :: [Int] -> [Int] -> [Int]
combine xs ys = if (lastS xs == headS ys) then appendS xs (tailS ys) else appendS xs ys

group :: [Int] -> [Int]
group xs = reduceS combine ([headS xs]) (mapS mapF (dropS xs 1))



-- mapF2 :: (a, b) -> [(a, [b])]
-- mapF2 (x,y) = [(x,[y])]

-- combineF :: [Int] -> [Int] -> [Int]
-- combineF xs ys = let (lv,la) = lastS xs
--                      (hv,ha) = headS ys
--                     -- tl@(tv,ta) = tailS ys
--                    in if (lv == hv) then appendS [(lv, (appendS la ha))] (tailS ys) else appendS xs ys

-- collectS :: [Int] -> [Int]
-- collectS xs = let (a,b) = headS xs in reduceS combineF ([a, [b]]) (mapS mapF2 (dropS xs 1))