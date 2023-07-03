import Seq
import ListSeq

combine :: (Int, String) -> (Int, String) -> (Int, String)
combine (p1, h1) (p2,h2) = (p1+p2, h2)

headS :: [(Int,String)] -> String
headS ff = let (CONS (pa, ha) b) = showlS ff in ha

alarma :: [(Int, String)] -> Int -> Maybe String
alarma xs v = let (partial, (pr, hr)) = scanS combine (0, "00:00") xs
                  ff = filterS (\(x,y) -> x >= v) partial
                in if lengthS ff > 0 then Just (headS ff) else if pr >= v then Just hr else Nothing
