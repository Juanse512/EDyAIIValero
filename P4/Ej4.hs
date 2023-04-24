data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show
memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT a E = False
memberRBT a (T _ l b r) | a == b = True
                        | a < b = memberRBT a l
                        | a > b = memberRBT a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
            where ins x E = T R E x E
                  ins x (T c l y r ) | x < y = balance c (ins x l) y r
                                     | x > y = balance c l y (ins x r )
                                     | otherwise = T c l y r

makeBlack E = E
makeBlack (T _ l x r ) = T B l x r

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r


insertLR :: Ord a => a -> RBT a -> RBT a
insertLR x t = makeBlack (ins x t)
            where ins x E = T R E x E
                  ins x (T c l y r ) | x < y = balanceL c (ins x l) y r
                                     | x > y = balanceR c l y (ins x r )
                                     | otherwise = T c l y r

balanceL :: Color -> RBT a -> a -> RBT a -> RBT a
balanceL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceL B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceL c l a r = T c l a r

balanceR :: Color -> RBT a -> a -> RBT a -> RBT a
balanceR B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceR B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceR c l a r = T c l a r
