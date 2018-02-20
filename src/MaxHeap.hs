-- this module should be imported qualified
module MaxHeap where

-- this could be made a generic heap by adding an additional parameter that is either GT or LT to indicate Max/Min heaps
-- the integer is the rank of the heap (no. of elements) for efficient heap construction
data MaxHeap a = Nil
               | Node Int a (MaxHeap a) (MaxHeap a)

rank :: MaxHeap a -> Int
rank (Node r _ _ _) = r
rank Nil = 0

-- should a min-heap be used instead? this seems to go the opposite direction that lazy evaluation would suggest
-- returns a (sorted) list
toList :: Ord a => MaxHeap a -> [a]
toList (Node _ x ls rs) = merge (toList ls) (toList rs) ++ x:[] where
  merge (l:ls) (r:rs)
    | l <= r    = l:(merge ls (r:rs))
    | otherwise = r:(merge (l:ls) rs)
  merge ls [] = ls
  merge [] rs = rs
toList Nil = []

-- inserts an element into the heap, preserving maximum-node property
-- the recursive insertion is done such that a new element goes into the smaller sub-heap
-- O(log n)
insert :: Ord a => a -> MaxHeap a -> MaxHeap a
insert x (Node rk y ls rs)
  | (rank ls) <= (rank rs)  = Node (succ rk) (max x y) (insert (min x y) ls) rs
  | (rank ls) > (rank rs)   = Node (succ rk) (max x y) ls (insert (min x y) rs)
insert x Nil = Node 1 x Nil Nil

-- O(n log n)
fromList :: Ord a => [a] -> MaxHeap a
fromList = foldr insert Nil
