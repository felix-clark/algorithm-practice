-- this module should be imported qualified
module MinHeap where

-- Note: this is NOT a complete min-heap, just enough to sort a list.
-- we need methods to insert a single element and remove only the min element.
-- these need bubble-up and bubble-down operations.
-- performant implementations typically use arrays.

-- this could be made a generic heap by adding an additional parameter that is either GT or LT to indicate Max/Min heaps
-- the integer is the rank of the heap (no. of elements) for efficient heap construction
data MinHeap a = Nil
               | Node Int a (MinHeap a) (MinHeap a)

rank :: MinHeap a -> Int
rank (Node r _ _ _) = r
rank Nil = 0

-- should a min-heap be used instead? this seems to go the opposite direction that lazy evaluation would suggest
-- returns a (sorted) list
toList :: Ord a => MinHeap a -> [a]
toList (Node _ x ls rs) = x : merge (toList ls) (toList rs) where
  merge (l:ls) (r:rs)
    | l <= r    = l:(merge ls (r:rs))
    | otherwise = r:(merge (l:ls) rs)
  merge ls [] = ls
  merge [] rs = rs
toList Nil = []

-- inserts an element into the heap, preserving maximum-node property
-- the recursive insertion is done such that a new element goes into the smaller sub-heap
-- O(log n)
insert :: Ord a => a -> MinHeap a -> MinHeap a
insert x (Node rk y ls rs)
  | (rank ls) <= (rank rs)  = Node (succ rk) (min x y) (insert (max x y) ls) rs
  | (rank ls) > (rank rs)   = Node (succ rk) (min x y) ls (insert (max x y) rs)
insert x Nil = Node 1 x Nil Nil

-- O(n log n)
fromList :: Ord a => [a] -> MinHeap a
fromList = foldr insert Nil
