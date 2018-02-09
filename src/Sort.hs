module Sort
    ( mergeSort,
      quickSort
    ) where

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
-- will use median of first, last, and middle as pivot to guard against common worst-case scenarios
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
-- quickSort [x] = [x] -- not strictly necessary
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
-- learn how to choose pivot simply
-- middle? random? median of 1st, last, middle?

-- this version of quicksort always uses the head as the pivot. easiest to write on its own, but will refer back to the generic quicksort.
quickSortPivotHead :: Ord a => [a] -> [a]
quickSortPivotHead [] = []
quickSortPivotHead (p:xs) = ltSorted ++ [p] ++ gteSorted where
  ltSorted = quickSort [a | a <- xs, a < p]
  gteSorted = quickSort [a | a <- xs, a >= p]

-- this version of quicksort always uses the last element as the pivot
quickSortPivotLast :: Ord a => [a] -> [a]
quickSortPivotLast [] = []
quickSortPivotLast ls = ltSorted ++ [p] ++ gteSorted where
  p = last ls
  xs = init ls
  -- (xs,[p]) = splitAt ((length ls) - 1) ls -- does this pattern matching work?
  ltSorted = quickSort [a | a <- xs, a < p]
  gteSorted = quickSort [a | a <- xs, a >= p]

-- this version of quicksort uses the middle element for a pivot
quickSortPivotMiddle :: Ord a => [a] -> [a]
quickSortPivotMiddle [] = []
quickSortPivotMiddle ls = ltSorted ++ [p] ++ gteSorted where
  (front,(p:back)) = splitAt ((length ls) `div` 2) ls
  xs = front ++ back
  ltSorted = quickSort [a | a <- xs, a < p]
  gteSorted = quickSort [a | a <- xs, a >= p]
  


-- selection sort is probably the most naive sorting algorithm.
-- it works well for short and nearly-sorted lists, but usually worse than insertion sort
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []

-- insertion sort is a decent simple choice for small lists
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []

-- bubble sort is really not great (n^2 complexity, and slow) except when the list is already nearly sorted.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []

