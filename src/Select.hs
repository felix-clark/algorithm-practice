module Select where

-- selection algorithms to return the ith-smallest element of a list

-- partition-based selection like quicksort.
-- O(n) average performance but poor worst-case (O(n^2) like quicksort).
-- using pivot at head results in O(n^2) performance for already-sorted lists.
quickSelect :: Ord a => Int -> [a] -> a
quickSelect i (p:xs)
  | i == cl = p
  | i < cl  = quickSelect i lt
  | i > cl  = quickSelect (i-cl-1) gte
  where
    ((lt,gte),cl) = partitionWithCount (< p) xs
    partitionWithCount :: (a -> Bool) -> [a] -> (([a],[a]),Int)
    partitionWithCount f xs = partCtIter (([],[]),0) f xs
    partCtIter res f [] = res
    partCtIter ((trues,falses),ctTr) f (x:xs)
      | f x       = partCtIter ((x:trues,falses),succ ctTr) f xs
      | otherwise = partCtIter ((trues,x:falses),ctTr) f xs
-- this case should not fire unless i > length input
quickSelect _ [] = error "order index out of range"


-- O(n) algorithm for finding *approximate* median.
-- guaranteed to not be in the top or bottom quartiles (a little better actually)
medianOfMedians :: Ord a -> [a] -> a
medianOfMedians x = let
  n = length x -- would be nice to not incur this additional O(n) operation
  medianOfFive = quickSelect 2 -- something simple like insertion sort then 2nd element might be preferable
  in
    undefined
medianOfMedians [] = error "cannot find median of empty list"


-- we can use the O(n) median-of-medians algorithm to write a quick-select algorithm with worst case O(n). It will not actually be faster in practice, except for extraordinarily long lists.
