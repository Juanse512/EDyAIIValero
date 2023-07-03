import Par

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

t2 = Join (Join (Leaf 10) (Leaf 15)) (Leaf 5) :: Tree Int
t3 = Join (Leaf 10) (Leaf 15) :: Tree Int

inorder2 E = []
inorder2 (Leaf a) = [a]
inorder2 (Join l r) = (inorder2 l) ++ (inorder2 r)

merge E t = t
merge t E = t
merge t t' = Join t t'

sufijos t = sufijos' t E

sufijos' E k = Leaf E
sufijos' (Leaf a) k = Leaf k
sufijos' (Join l r) k = let (l', r') = sufijos' l (merge r k) ||| sufijos' r k
                       in merge l' r'

conSufijos t = conSufijos' t (sufijos t)

conSufijos' (Leaf a) (Leaf b) = Leaf (a, b)
conSufijos' (Join l r) (Join a b) = let (l', r') = conSufijos' l a ||| conSufijos' r b
                                   in Join l' r'

maxT t = reduce (max) (minBound::Int) t 

maxAll t = maxT (mapT2 maxT t)

mejorGanancia t = maxAll t''
    where
        t' = conSufijos t
        t'' = mapT2 sub t'

        sub (a, t) = mapT2 (\i -> i - a) t


reduce f b E = b
reduce f b t = f b (reduce' f t)
    where
        reduce' f (Leaf a) = a
        reduce' f (Join l r) = f (reduce' f l) (reduce' f r)

mapT2 f E = E
mapT2 f (Leaf a) = Leaf (f a)
mapT2 f (Join l r) = let (l', r') = mapT2 f l ||| mapT2 f r
                     in Join l' r'