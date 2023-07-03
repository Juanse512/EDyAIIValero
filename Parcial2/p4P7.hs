import Control.Parallel

infix 1 |||

(|||)   ::   a -> b -> (a,b)
a ||| b = a `par` b `par` (a,b)


data Paren = Open | Close

arr1 = [Open,Open,Close,Close]
arr2 = [Close,Open,Open,Close,Close,Open]


matchP :: [Paren] -> (Int, Int)
matchP [] = (0, 0)
matchP (Close:[]) = (1,0)
matchP (Open:[]) = (0,1)
matchP xs = let ((i,j),(i',j'))  = matchP (take (div (length xs) 2) xs) |||  matchP (drop (div (length xs) 2) xs)
                k = j - i'
            in if k > 0 then (i, k+j') else (i-k, j')


matchParen s = (0,0) == (matchP s)

--  B -------------------------------------------
contraction :: (a -> a -> a) -> [a] -> [a]
contraction f [] = []
contraction f [x] = [x]
contraction f (x:y:xs) = let (x',xs') =  (f x y) |||  (contraction f xs )
                         in x' : xs'
reduce :: (a -> a -> a)-> a -> [a] -> a
reduce f b [] = b
reduce f b [x] = f b x
reduce f b xs = reduce f b $ contraction f xs

f (i,j) (i',j') = if ( j - i') > 0 then (i, ( j - i')+j') else (i-( j - i'), j')

isOpen (Open) = True
isOpen _ = False

matchParen2 s = reduce f (0,0) (map (\i -> if isOpen i then (0,1) else (1,0)) s)