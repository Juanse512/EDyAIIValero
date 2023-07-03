
import Seq
import ListSeq


sccmlAux :: [Int] -> (Int,Int,Int,Int,Int,Int)
sccmlAux [] = (0,0,0,0,0,0)
sccmlAux [x] = (x,x,1,1,1,1)
sccmlAux xs = let 
                (NODE l r) = showtS xs
                ((x1,y1, p1, s1, t1, tt1), (x2,y2, p2, s2, t2, tt2)) = (sccmlAux l, sccmlAux r)
                x = x1
                y = y2
                p = if p1 == tt1 && y1 < x2 then p1+p2 else p1
                s = if s2 == tt2 && x2 > y1 then s2+s1 else s2
               
                mm = if y1 < x2 then s1+p2 else 0
                mt = max (max t1 t2) mm
                t = max (max p s) mt
                
                tt = tt1+tt2

                in (x,y,p,s,t, tt)

sccml :: [Int] -> Int
sccml xs = let (x,y,p,s,t,tt) = sccmlAux xs in t-1

tabulateF :: [Int] -> Int -> (Int,Int)
tabulateF xs x = if (lengthS xs - 1) == x then (1,1) else if (nthS xs x) < (nthS xs (x+1)) then (1,1) else (0,0)


combine :: (Int, Int) -> (Int, Int) -> (Int, Int)
combine (v1, s1) (v2, s2) = if v2 == 1 then (v1, s1+s2) else (v2,s2)

mapF :: (Int, Int) -> Int
mapF (a,b) = b

longestStreak :: [Int] -> Int
longestStreak xs = let (partial, res) = scanS combine (0,0) ((tabulateS (tabulateF xs) (lengthS xs))::[(Int,Int)]) in reduceS max 0 (mapS mapF partial)


longestStreak2 :: [(Int,Int)] -> [(Int,Int)]
longestStreak2 xs = let (partial, res) = scanS combine (0,0) xs in partial

