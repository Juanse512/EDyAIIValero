import Seq
import ListSeq

data Tree a = E | L a | N Int (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size E = 0
size (L a) = 1
size (N v l r) = v


concatT :: Tree (Tree a) -> Tree a
concatT E = E
concatT (L t) = t
concatT (N v l r) = let (ll, rr) = (concatT l, concatT r) in (N ((size ll) + (size rr)) ll rr)


trimR :: Int -> Tree a -> Tree a
trimR v E = E
trimR v (L x) = (L x)
trimR v (N s l r) = if v == (size l) then l else if v < (size l) then (trimR v l) else (N s l (trimR (v-(size l)) r))

trimL :: Int -> Tree a -> Tree a
trimL v E = E
trimL v (L x) = (L x)
trimL v (N s l r) = if v == (size l) then r else if v < (size l) then (N s (trimL v l) r) else (trimL (v-(size l)) r)

subq :: Tree a -> Int -> Int -> Tree a
subq t s e = trimL s (trimR e t)

tabF :: [Int] -> Int -> [Int]
tabF xs v = if lengthS (filterS (\x -> x == (nthS xs v)) (dropS xs (v+1))) == 0 then [nthS xs v] else []


uniquify :: [Int] -> [Int]
uniquify xs = joinS (tabulateS (tabF xs) (lengthS xs))

