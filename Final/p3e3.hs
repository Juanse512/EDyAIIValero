data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL :: CList a -> a
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit a) = EmptyCL
tailCL (Consnoc x (CUnit v) y) = Consnoc v EmptyCL y
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x y z) = Consnoc (headCL y) (tailCL y) z

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x y z) = Consnoc z (reverseCL y) x