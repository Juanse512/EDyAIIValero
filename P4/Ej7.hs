data PHeap a = Empty | Root a [PHeap a] deriving Show
compareRoots :: Ord a => PHeap a -> PHeap a -> Bool
compareRoots (Root v1 _) (Root v2 _) = v1 < v2

isPheap :: Ord a => PHeap a -> Bool
isPheap Empty = True
isPheap (Root v []) = True
isPheap (Root v (x:xs)) = if (compareRoots (Root v []) x) then (isPheap x) && isPheap (Root v xs) else False

mergeHeap :: Ord a => PHeap a -> PHeap a -> PHeap a
mergeHeap Empty v2 = v2
mergeHeap v1 Empty = v1
mergeHeap (Root v1 q1) (Root v2 q2) = if v1 > v2 then (Root v2 ((Root v1 q1):q2)) else (Root v1 ((Root v2 q2):q1))

insert :: Ord a => PHeap a -> a -> PHeap a
insert v x = mergeHeap v (Root x []) 

concatHeaps :: Ord a => [PHeap a] -> PHeap a
concatHeaps [] = Empty
concatHeaps [x] = x
concatHeaps (x:xs) = mergeHeap x (concatHeaps xs)

getMin :: Ord a => [PHeap a] -> PHeap a -> PHeap a
getMin [] m = m
getMin (x:xs) Empty = getMin xs x
getMin (x:xs) m = if compareRoots x m then getMin xs x else getMin xs m

comparePHeaps :: Ord a => PHeap a -> PHeap a -> Bool
comparePHeaps Empty v = False
comparePHeaps v Empty = False
comparePHeaps (Root v1 x1) (Root v2 x2) = v1 == v2

deletePHeap :: Ord a => [PHeap a] -> PHeap a -> [PHeap a]
deletePHeap [] _ = []
deletePHeap [x] v = if comparePHeaps x v then [] else [x]
deletePHeap (x:xs) v = if comparePHeaps x v then xs else (x:(deletePHeap xs v))

getRoot :: Ord a => PHeap a -> a
getRoot (Root a _) = a

delMin :: Ord a => PHeap a -> Maybe (a, PHeap a)
delMin Empty = Nothing
delMin (Root a l) = let m = getMin l Empty in Just (a, (Root (getRoot m) (deletePHeap l m)))