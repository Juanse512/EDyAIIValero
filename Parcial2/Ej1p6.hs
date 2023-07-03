data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show




nth :: BTree a -> Int -> a
nth (Node _ Empty v Empty) index = v
nth (Node i Empty v (Node i2 l2 v2 r2)) index = if index == i then v else nth (Node i2 l2 v2 r2) index
nth (Node i (Node i1 l1 v1 r1) v Empty) index = if index == i then v else nth (Node i1 l1 v1 r1) index
nth (Node i (Node i1 l1 v1 r1) v (Node i2 l2 v2 r2)) index = if index == i then v else
                                                             if index > i then nth (Node i2 l2 v2 r2) (index - i1)
                                                             else nth (Node i1 l1 v1 r1) (index)