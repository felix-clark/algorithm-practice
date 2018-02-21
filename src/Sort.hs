module Sort
  (  isSorted,
     mergeSort,
     quickSort',
     quickSort,
     heapSort,
     bubbleSort,
     selectionSort,
     insertionSort,
  ) where

-- allow built-in structures and functions only (no external packages), like (linked) lists.
-- these are (at time of writing) all simple O(n) functions.
import Data.List (partition, delete, insert)
-- import our implementation of MaxHeap for heapsort
import qualified MaxHeap
-- we will import Data.Vector to use a quicksort version with median-of-three pivot choice
-- without resorting to compiler intrinsics.
import qualified Data.Vector as Vec

-- test whether a list is sorted with a single pass-through. O(n) time complexity
isSorted :: Ord a => [a] -> Bool
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
isSorted _ = True -- lists of 0 or 1 elements are sorted

-- with a functional programming approach, many of these algorithms will not exactly be optimal, especially in space complexity.
-- C sorting algorithms often make heavy use of swap, which is not so pretty in a lazy context.
-- converting to a Data.Vector could help with this, but we should instead implement our own data structures used here.
-- note that implementation of constant-time access structures could necessitate some fancy compiler intrinsics.

-- merge sort is probably the simplest sorting algorithm with linearithmic complexity.
-- it maintains linearithmic time complexity even in the worst case (unlike quicksort)
-- the splitAt being O(n) does slow it down, but should maintain linearithmic complexity.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSorted (mergeSort front) (mergeSort back) where
  (front,back) = splitAt (length xs `div` 2) xs
  mergeSorted :: Ord a => [a] -> [a] -> [a]
  mergeSorted [] bs = bs
  mergeSorted as [] = as
  mergeSorted (a:as) (b:bs)
    | a <= b    = a:(mergeSorted as (b:bs))
    | otherwise = b:(mergeSorted (a:as) bs)


-- note that quicksort has n^2 time complexity in the worst case (e.g. using 1st element as pivot for already-sorted list)
-- pathological examples could still be constructed to make this version O(n^2)
-- a random pivot does well on average but we won't introduce that complexity.

-- this version always uses the head as pivot.
-- it will do poorly in already-sorted lists but has a simple implementation
-- and avoids random-access of the linked list.
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = quickSort lt ++ p : quickSort gte where
  (lt,gte) = partition (< p) xs
  
-- this version converts to a Data.Vector, which has O(1) random access.
-- this lets us use a median-of-three pivot without increasing the order of the time complexity.
quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' x = let
  vecx = Vec.fromList x -- O(n) conversion
  in Vec.toList $ quickSortVec vecx

quickSortVec :: Ord a => Vec.Vector a -> Vec.Vector a
quickSortVec x
  | Vec.null x = Vec.empty
  | otherwise  = let
      h = Vec.head x
      m = x Vec.! ((Vec.length x) `div` 2)
      l = Vec.last x
    -- prefer head pivot, then last pivot, then middle. not sure which way middle/last matters
      quickSortChoice = if (m <= h && h <= l) || (l <= h && h <= m)
                        then quickSortVecPivotHead
                        else if (h <= l && l <= m) || (m <= l && l <= h)
                             then quickSortVecPivotLast
                             else quickSortVecPivotMiddle
    in quickSortChoice x

-- this version of quicksort always uses the head as the pivot. easiest to write on its own, but will refer back to the generic quicksort.
-- if we knew the data is random, using this version recursively would be the best choice, since random access on a linked list is O(n) not O(1).
-- these are actually all guaranteed to be non-empty by the logic of quickSortVec, so we can probably remove this check
quickSortVecPivotHead :: Ord a => Vec.Vector a -> Vec.Vector a
quickSortVecPivotHead x = quickSortVec lt Vec.++ (Vec.cons p (quickSortVec gte)) where
  (lt,gte) = Vec.unstablePartition (< p) xs
  p = Vec.head x
  xs = Vec.tail x

-- -- this version of quicksort always uses the last element as the pivot
quickSortVecPivotLast :: Ord a => Vec.Vector a -> Vec.Vector a
quickSortVecPivotLast x = quickSortVec lt Vec.++ (Vec.cons p (quickSortVec gte)) where
  (lt,gte) = Vec.unstablePartition (< p) xs
  p = Vec.last x
  xs = Vec.init x

-- -- this version of quicksort uses the middle element for a pivot
quickSortVecPivotMiddle :: Ord a => Vec.Vector a -> Vec.Vector a
quickSortVecPivotMiddle x = quickSortVec lt Vec.++ (Vec.cons p (quickSortVec gte)) where
  (front,back) = Vec.splitAt (Vec.length x `div` 2) x
  p = Vec.head back
  xs = front Vec.++ (Vec.tail back)
  (lt,gte) = Vec.unstablePartition (< p) xs

-- converts to and from our max-heap implementation to retrieve a sorted list.
-- a min-heap would also work just as well, i think.
heapSort :: Ord a => [a] -> [a]
heapSort = MaxHeap.toList . MaxHeap.fromList

-- this is a fast overall sort that uses quicksort until some optimal depth then switches to heapsort.
-- it is used in many STL implementations. note that neither quicksort or heapsort are stable.
introSort :: Ord a => [a] -> [a]
introSort = undefined

-- selection sort is probably the most naive sorting algorithm.
-- it works decently enough for short and nearly-sorted lists, but usually worse than insertion sort.
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort ls = x : selectionSort xs where
  x = minimum ls
  xs = delete x ls

-- insertion sort is a decent simple choice for small lists
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert' []
-- insert takes an element and a list, and returns the list with the element inserted before the first other element that is >= than it.
-- a hand-written version looks like:
  where
    insert' y (x:xs)
      | y <= x    = (y:x:xs)
      | otherwise = x : insert' y xs
    insert' x [] = x:[]

-- bubble sort is usually not great. n^2 complexity, and worse than insertion and selection.
-- the one context in which it enjoys an advantage is when the list is already nearly sorted.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort ls = if flips == 0 then possSort else bubbleSort possSort where
  (possSort,flips) = iterBubble ls 0
-- the iterative part of this method returns a tuple of the list and a count of flips.
  iterBubble :: Ord a => [a] -> Int -> ([a],Int)
  iterBubble (x:y:xs) flips
    | x <= y    = let (possSorted,flips') = iterBubble (y:xs) flips
                  in (x:possSorted,flips')
    | otherwise = let (possSorted,flips') = iterBubble (x:xs) flips
                  in (y:possSorted,succ flips')
  iterBubble x flips = (x,flips)
-- it should be better to keep track of whether any flips have occurred as opposed to e.g. checking whether it is sorted every time (which might actually make it O(n^3)??) like so:
-- bubbleSort' (x:y:xs) = if isSorted possSort then possSort else bubbleSort' possSort
--   where possSort
--           | x <= y    = x:(bubbleSort' (y:xs))
--           | otherwise = y:(bubbleSort' (x:xs))
-- bubbleSort' x = x
-- another approach would be to iterate (length sublist) times on each sublist, but this would nullify the one best-case advantage that bubble sort has.
    
