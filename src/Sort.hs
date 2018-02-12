module Sort
    ( mergeSort,
      quickSort,
      bubbleSort,
      insertionSort,
      isSorted
    ) where

import Data.List (partition, insert)

-- test whether a list is sorted with a single pass-through. O(n) time complexity
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:xs) = (x <= head xs) && (isSorted xs)


-- with a functional programming approach, many of these algorithms will not exactly be optimal, especially in space complexity.
-- C sorting algorithms often make heavy use of swap, which is not so pretty in a lazy context.

-- merge sort is probably the simplest sorting algorithm with linearithmic complexity.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSorted (mergeSort front) (mergeSort back) where
  (front,back) = splitAt n xs
  n = (length xs) `div` 2
  mergeSorted [] bs = bs
  mergeSorted as [] = as
  mergeSorted (a:as) (b:bs)
    | a <= b    = a:(mergeSorted as (b:bs))
    | otherwise = b:(mergeSorted (a:as) bs)

-- this will not likely be the "no extra memory" version, but should still have good linearithmic complexity on average.
-- note that it has n^2 time complexity in the worst case (e.g. using 1st element as pivot for already-sorted list)
-- will use median of first, last, and middle as pivot to guard against common worst-case scenarios.
-- a random pivot does well on average but we won't introduce that complexity.
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort x = let
  h = head x
  m = x !! ((length x) `div` 2)
  l = last x
  -- prefer head pivot, then last pivot, then middle. not sure which way middle/last matters
  quickSortChoice = if (m <= h && h <= l) || (l <= h && h <= m)
                    then quickSortPivotHead
                    else if (h <= l && l <= m) || (m <= l && l <= h)
                         then quickSortPivotLast
                         else quickSortPivotMiddle
  in quickSortChoice x

-- this version of quicksort always uses the head as the pivot. easiest to write on its own, but will refer back to the generic quicksort.
quickSortPivotHead :: Ord a => [a] -> [a]
quickSortPivotHead [] = []
quickSortPivotHead (p:xs) = quickSort lt ++ [p] ++ quickSort gte where
  (lt,gte) = quickSort <$> partition (< p) xs

-- this version of quicksort always uses the last element as the pivot
quickSortPivotLast :: Ord a => [a] -> [a]
quickSortPivotLast [] = []
quickSortPivotLast ls = quickSort lt ++ [p] ++ quickSort gte where
  p = last ls
  xs = init ls
  -- (xs,[p]) = splitAt ((length ls) - 1) ls -- does this pattern matching work?
  (lt,gte) = partition (< p) xs

-- this version of quicksort uses the middle element for a pivot
quickSortPivotMiddle :: Ord a => [a] -> [a]
quickSortPivotMiddle [] = []
quickSortPivotMiddle ls = quickSort lt ++ [p] ++ quickSort gte where
  (front,(p:back)) = splitAt ((length ls) `div` 2) ls
  (lt,gte) = partition (< p) (front ++ back)


-- selection sort is probably the most naive sorting algorithm.
-- it works well for short and nearly-sorted lists, but usually worse than insertion sort
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []

-- insertion sort is a decent simple choice for small lists
insertionSort :: Ord a => [a] -> [a]
-- insertionSort [] = []
insertionSort = foldr insert []
-- this feels like cheating, so let's write our own version of insert to show what's going on

-- bubble sort is really not great. n^2 complexity, and worse than insertion and selection.
-- the one context it enjoys an advantage in is when the list is already nearly sorted.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
