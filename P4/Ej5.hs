data UDT a = Hoja | DNode a (UDT a) (UDT a) | TNode a a (UDT a) (UDT a) (UDT a) | CNode a a a (UDT a) (UDT a) (UDT a) (UDT a) deriving Show
data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show


rbtToUdt :: Ord a => RBT a -> UDT a 
rbtToUdt E = Hoja

rbtToUdt (T B (T R l1 v1 r1) a (T R l2 v2 r2)) = (CNode a v1 v2 (rbtToUdt l1) (rbtToUdt l2) (rbtToUdt r1) (rbtToUdt r2))
rbtToUdt (T B (T R l1 v1 r1) a r) = (TNode a v1 (rbtToUdt l1) (rbtToUdt r1) (rbtToUdt r))
rbtToUdt (T B l a (T R l2 v2 r2)) = (TNode a v2 (rbtToUdt l) (rbtToUdt l2) (rbtToUdt r2))
rbtToUdt (T B l a r) = (DNode a (rbtToUdt l) (rbtToUdt r))  
