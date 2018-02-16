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
isSorted (x:y:xs) = (x <= y) && (isSorted (y:xs))
isSorted _ = True -- lists of 0 or 1 elements are sorted

-- with a functional programming approach, many of these algorithms will not exactly be optimal, especially in space complexity.
-- C sorting algorithms often make heavy use of swap, which is not so pretty in a lazy context.

-- merge sort is probably the simplest sorting algorithm with linearithmic complexity.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSorted (mergeSort front) (mergeSort back) where
  (front,back) = splitAt (length xs `div` 2) xs
  mergeSorted :: Ord a => [a] -> [a] -> [a]
  -- mergeSorted [] = id
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
  (xs,[p]) = splitAt (length ls - 1) ls
  (lt,gte) = partition (< p) xs

-- this version of quicksort uses the middle element for a pivot
quickSortPivotMiddle :: Ord a => [a] -> [a]
quickSortPivotMiddle [] = []
quickSortPivotMiddle ls = quickSort lt ++ [p] ++ quickSort gte where
  (front,(p:back)) = splitAt (length ls `div` 2) ls
  (lt,gte) = partition (< p) (front ++ back)


-- selection sort is probably the most naive sorting algorithm.
-- it works well for short and nearly-sorted lists, but usually worse than insertion sort
selectionSort :: Ord a => [a] -> [a]
selectionSort = undefined

-- insertion sort is a decent simple choice for small lists
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
-- this feels like cheating, so let's write our own version of insert to show what's going on

-- bubble sort is usually not great. n^2 complexity, and worse than insertion and selection.
-- the one context it enjoys an advantage in is when the list is already nearly sorted.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort ls = if flips == 0 then possSort else bubbleSort possSort where
  (possSort,flips) = iterBubble ls 0
  iterBubble :: Ord a => [a] -> Int -> ([a],Int)
  iterBubble (x:y:xs) flips
    | x <= y    = let (possSorted,flips') = iterBubble (y:xs) flips
                  in (x:possSorted,flips')
    | otherwise = let (possSorted,flips') = iterBubble (x:xs) flips
                  in (y:possSorted,succ flips')
  iterBubble x flips = (x,flips)
-- the iterative part of this method returns a tuple of the list and a count of flips.
-- it should be better to keep track of whether any flips have occurred as opposed to e.g. checking whether it is sorted every time (which might actually make it O(n^3)??) like so:
-- bubbleSort' (x:y:xs) = if isSorted possSort then possSort else bubbleSort possSort
--   where possSort
--           | x <= y    = x:(bubbleSort (y:xs))
--           | otherwise = y:(bubbleSort (x:xs))
-- bubbleSort' x = x
    
