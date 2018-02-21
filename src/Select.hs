module Select where

-- selection algorithms to return the ith-smallest element of a list

-- partition-based selection like quicksort.
-- good average performance (O(n)) but poor worst-case (O(n^2) like quicksort).
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

