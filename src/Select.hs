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
    ((lt,gte),cl) = partCtIter (([],[]),0) (< p) xs
    partCtIter res f [] = res
    partCtIter ((trues,falses),ctTr) f (y:ys)
      | f y       = partCtIter ((y:trues,falses),succ ctTr) f ys
      | otherwise = partCtIter ((trues,y:falses),ctTr) f ys
-- this case should not fire unless i > length input
quickSelect _ [] = error "order index out of range"

-- quick select with a random pivot
quickSelectR :: Ord a => Int -> [a] -> a
quickSelectR = undefined

-- O(n) algorithm for finding *approximate* median.
-- guaranteed to not be in the top or bottom quartiles (a little better: middle ~40% for sufficiently long inputs)
medianOfMedians :: Ord a => [a] -> a
medianOfMedians [] = error "cannot find median of empty list"
medianOfMedians x = quickSelect' (length medianList `div` 2) medianList
  where
    medianList = getMedians x
    getMedians [] = []
    getMedians x = medianOfSmall front : getMedians back
      where
        (front,back) = splitAt 5 x
        medianOfSmall [] = error "shouldn't be here"
        medianOfSmall x = quickSelect (length x `div` 2) x -- something simple like insertion sort then 2nd element might be preferable
      

-- we can use the O(n) median-of-medians algorithm to write a quick-select algorithm with worst case O(n).
-- It will not actually be faster in practice, except for extraordinarily long lists, as there are many mutually recursive calls.
-- it's actually typically best in practice to choose a *random* pivot. we should experiment with this in haskell.
quickSelect' :: Ord a => Int -> [a] -> a
quickSelect' _ [] = error "order index out of range"
quickSelect' i x
  | i == cl = p
  | i < cl  = quickSelect' i lt
  | i > cl  = quickSelect' (i-cl-1) gte
  where
    p = medianOfMedians x -- linear time to find a good pivot, but still intensive
    ((lt,gte),cl) = partitionAndRemovePIter (([],[]),0) p x -- the logic is a bit more complicated because we need to remove the pivot when it shows up so we don't keep checking it.
    -- we should never hit the base case of an empty set since the pivot should be guaranteed to exist in the list.
    partitionAndRemovePIter ((trues,falses),ctTr) p (x:xs)
      | p == x    = partCtIter ((trues,falses),ctTr) (< p) xs
      | x < p     = partitionAndRemovePIter ((x:trues,falses),succ ctTr) p xs
      | x >= p    = partitionAndRemovePIter ((trues,x:falses),ctTr) p xs
    partitionAntRemovePIter _ _ [] = error "the pivot is supposed to exist in the list!"
    partCtIter res f [] = res
    partCtIter ((trues,falses),ctTr) f (x:xs)
      | f x       = partCtIter ((x:trues,falses),succ ctTr) f xs
      | otherwise = partCtIter ((trues,x:falses),ctTr) f xs
