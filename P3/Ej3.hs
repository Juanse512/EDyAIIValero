data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCl :: CList a -> a
headCl (CUnit x) = x
headCl (Consnoc x l y) = x

tailCl :: CList a -> CList a
tailCl (CUnit x) = EmptyCL
tailCl (Consnoc x EmptyCL y) = CUnit y
tailCl (Consnoc x l y) = Consnoc (headCl l) (tailCl l) y

isEmptyCl :: CList a -> Bool
isEmptyCl EmptyCL = True
isEmptyCl _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _ = False

reverseCL ::  CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x l z) = Consnoc z (reverseCL l) x

inits :: CList a -> [a]
inits EmptyCL = []
inits (CUnit x) = [x]
inits (Consnoc x l z) = x : inits l

lasts :: CList a -> [a]
lasts EmptyCL = []
lasts (CUnit x) = [x]
lasts (Consnoc x l z) = z : lasts l

