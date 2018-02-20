module Sort
  (  isSorted,
     mergeSort,
     quickSort,
     heapSort,
     bubbleSort,
     selectionSort,
     insertionSort,
  ) where

-- allow built-in structures and functions only (no external packages), like (linked) lists.
-- these are (at time of writing) all simple O(n) functions.
import Data.List (partition, delete, insert)
import qualified MaxHeap

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

-- note that it has n^2 time complexity in the worst case (e.g. using 1st element as pivot for already-sorted list)
-- it could use median of first, last, and middle as pivot to guard against common worst-case scenarios, but haskell lists do not have O(1) random access.
-- could convert to a Vector then sort, but we need to implement that data structure first (or cheat by importing it).
-- pathological examples could still be constructed to make this version O(n^2)
-- a random pivot does well on average but we won't introduce that complexity.
quickSort :: Ord a => [a] -> [a]
quickSort = quickSortPivotHead
-- the below is included for visibility but is not efficient on linked lists. re-visit this with Vector
-- quickSort [] = []
-- quickSort x = let
--   h = head x
--   m = x !! ((length x) `div` 2)
--   l = last x
--   -- prefer head pivot, then last pivot, then middle. not sure which way middle/last matters
--   quickSortChoice = if (m <= h && h <= l) || (l <= h && h <= m)
--                     then quickSortPivotHead
--                     else if (h <= l && l <= m) || (m <= l && l <= h)
--                          then quickSortPivotLast
--                          else quickSortPivotMiddle
--   in quickSortChoice x

-- this version of quicksort always uses the head as the pivot. easiest to write on its own, but will refer back to the generic quicksort.
-- if we knew the data is random, using this version recursively would be the best choice, since random access on a linked list is O(n) not O(1).
quickSortPivotHead :: Ord a => [a] -> [a]
quickSortPivotHead [] = []
quickSortPivotHead (p:xs) = quickSort lt ++ p : quickSort gte where
  (lt,gte) = partition (< p) xs

-- this version of quicksort always uses the last element as the pivot
quickSortPivotLast :: Ord a => [a] -> [a]
quickSortPivotLast [] = []
quickSortPivotLast ls = quickSort lt ++ p : quickSort gte where
  (xs,[p]) = splitAt (length ls - 1) ls
  (lt,gte) = partition (< p) xs

-- this version of quicksort uses the middle element for a pivot
quickSortPivotMiddle :: Ord a => [a] -> [a]
quickSortPivotMiddle [] = []
quickSortPivotMiddle ls = quickSort lt ++ p : quickSort gte where
  (front,(p:back)) = splitAt (length ls `div` 2) ls
  (lt,gte) = partition (< p) (front ++ back)

-- come back to this once we implement an ordered heap
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
    
