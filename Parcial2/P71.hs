import Seq
import ListSeq



mapF :: Int -> (Int, Int)
mapF x = (x,1)





combine :: (Int, Int) -> (Int, Int) -> (Int, Int)
combine (x1,c1) (x2,c2) = (x1+x2, c1+c2)


avg :: (Int, Int) -> Int
avg (0,0) = 0
avg (a,b) = div a b

promedios :: [Int] -> [Int]
promedios xs = let (partial, res) = scanS combine (0,0) (mapS mapF xs) in (appendS (mapS avg partial) [(avg res)])



tabF :: [Int] -> [Int] -> Int -> Int
tabF xs maxxs v = if (nthS xs v) > (nthS maxxs v) then 1 else 0

mayores :: [Int] -> Int
mayores xs = let (partial, res) = scanS max 0 xs in (reduceS (+) 0 ((tabulateS (tabF xs (appendS partial [res])) ((lengthS xs)))::[Int])) - 1
