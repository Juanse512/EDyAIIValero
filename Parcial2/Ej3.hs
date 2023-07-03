import Seq
import ListSeq

tabulateF :: [Float] -> Float -> Int -> (Int, Bool, Int)
tabulateF xs v x = if (nthS xs x) > v then (x, True, x) else (x, False, x)

maxA :: Int -> Int -> Int
maxA a b = if a > b then a else b

mapF :: (Int, Bool, Int) -> Int
mapF (x,y,z) = if z-x == 0 && y == False then 0 else z - x + 1

combine :: (Int, Bool, Int) -> (Int, Bool, Int) -> (Int, Bool, Int)
combine (i1, b1, e1) (i2, b2, e2) = if b1 == True && b2 == True && e1 == (i2 - 1) then (i1, True, e2) else (i2, b2, e2)

longestStreak :: [Float] -> Float -> Int
longestStreak xs v = let (partial, res) = scanS (combine) (0,False,0) ((tabulateS (tabulateF xs v) (lengthS xs))::[(Int, Bool, Int)])
                     in reduceS maxA 0 (mapS mapF partial)


-- vacio = vacio
-- insertar x v vacio = (insertar x v vacio)
-- insertar x v (insertar x v1 m) = insertar x v1 m
-- insertar x v (insertar x1 v1 m) = insertar x1 v1 (insertar x v m) 

