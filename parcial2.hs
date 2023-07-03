data Treap p k = E | N (Treap p k) p k (Treap p k)

key :: Treap p k -> k
key (N _ p k _) = k

priority :: Treap p k -> p
priority (N _ p k _) = p

isTreap :: (Ord k, Ord p) => Treap p k -> Bool
isTreap t = isBst t && isHeap t

isBst :: (Ord k, Ord p) => Treap p k -> Bool
isBst E = True
isBst (N E p k b) = if minimum b > k then isBst b else False
isBst (N a p k E) = if maximum a < k then isBst a else False
isBst (N a p k b) = if minimum b > k && maximum a < k then isBst a && isBst b else False

maximum :: (Ord k, Ord p) => Treap p k -> k
maximum (N a p k E) = k
maximum (N a p k b) = maximum b

minimum :: (Ord k, Ord p) => Treap p k -> k
minimum (N E p k b) = k
minimum (N a p k b) = minimum a

isHeap :: (Ord k, Ord p) => Treap p k -> Bool
isHeap E = True
isHeap (N E p k E) = True
isHeap (N E p k h1@(N _ p1 k1 _)) = if p > p1 then isHeap h1 else False
isHeap (N h1@(N _ p1 k1 _) p k E) = if p > p1 then isHeap h1 else False
isHeap (N l p k r) = if (p > priority l) && (p > priority r) then isHeap r && isHeap l else False