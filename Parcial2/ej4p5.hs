import Seq
import ListSeq
data Paren = Open | Close


mapF :: Paren -> Int
mapF Open = 1
mapF Close = -1

reduceF :: Int -> Int -> Int
reduceF a b = if a == -1 || b == -1 then -1 else 0

matchP :: [Paren] -> Bool
matchP xs = let (partial, res) = scanS (+) 0 (mapS mapF xs) in (reduceS reduceF 0 partial) >= 0 && res == 0