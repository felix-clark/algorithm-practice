-- this is a functionally and syntactically inferior version of Haskell built-in lists.
-- it's only here for fun and practice, and is not intended to actually be used instead of built-ins.
-- we can at least implement versions of the built-in functions used elsewhere
module LinkedList where

data LinkedList a = Nil | Cons a (LinkedList a)

-- O(1)
myHead :: LinkedList a -> a
myHead (Cons x _) = x
 -- could let the "non-exhaustive pattern" error fly, but it's better to be explicit
myHead Nil = error "empty LinkedList has no head"

-- O(1)
myTail :: LinkedList a -> LinkedList a
myTail (Cons _ xs) = xs
myTail Nil = error "empty LinkedList has no tail"

-- O(n) time (yes, this does have to iterate over the entire list)
myLength :: LinkedList a -> Int
myLength Nil = 0
myLength (Cons x xs) = 1 + (myLength xs)

-- O(n) time (this is why random access is not O(1) in built-in lists in haskell)
myIndex :: Int -> LinkedList a -> a
myIndex 0 l = myHead l
myIndex n (Cons x xs) = myIndex (n-1) xs
-- negative indices will loop forever

-- O(n): returns last element
myLast :: LinkedList a -> a
myLast (Cons x Nil) = x
myLast (Cons _ xs) = myLast xs
myLast Nil = error "empty LinkedList has no last element"
  
-- O(n): returns list of all but last element
myInit :: LinkedList a -> LinkedList a
myInit (Cons x (Cons _ Nil)) = (Cons x Nil)
myInit (Cons x xs) = Cons x (myInit xs)
myInit Nil = error "empty LinkedList has no init"

-- conversion to a built-in list
myToList :: LinkedList a -> [a]
myToList (Cons x xs) = (x:myToList xs)
myToList Nil = []

-- conversion from a built-in list
myFromList :: [a] -> LinkedList a
myFromList (x:xs) = Cons x (myFromList xs)
myFromList [] = Nil

myFind :: (a -> Bool) -> LinkedList a -> Maybe a
myFind pred (Cons x xs)
  | pred x    = Just x
  | otherwise = myFind pred xs
myFind _ Nil = Nothing

myFilter :: (a -> Bool) -> LinkedList a -> LinkedList a
myFilter pred (Cons x xs)
  | pred x    = Cons x (myFilter pred xs)
  | otherwise = myFilter pred xs
myFilter _ Nil = Nil

-- it's trivial to implement a version that traverses the list twice, but the whole point of this function is to do it only once
-- equivalent to Data.List.partition
myPartition :: (a -> Bool) -> LinkedList a -> (LinkedList a, LinkedList a)
myPartition = partIter (Nil,Nil) where
  partIter :: (LinkedList a, LinkedList a) -> (a -> Bool) -> LinkedList a -> (LinkedList a, LinkedList a)
  partIter (trues,falses) pred (Cons x xs)
    | pred x    = partIter ((Cons x trues),falses) pred xs
    | otherwise = partIter (trues,(Cons x falses)) pred xs
  partIter (trues,falses) _ Nil = (trues,falses)

-- removes the first instance of an element from a list.
-- if the element does not exist, returns the original list.
-- equivalent to Data.List.delete; used in selectionSort.
myDelete :: Eq a => a -> LinkedList a -> LinkedList a
myDelete y (Cons x xs)
  | y == x    = xs
  | otherwise = Cons x (myDelete y xs)
myDelete _ Nil = Nil

-- this does not insert before the head (which is trivial with the constructor),
--  but rather before the first element greater than or equal to it.
-- equivalent to Data.List.insert; used in insertionSort.
-- because this LinkedList is an instance of Foldable, insertion sort should be trivially implementable as "insertionSort = foldr myInsert Nil"
myInsert :: Ord a => a -> LinkedList a -> LinkedList a
myInsert y (Cons x xs)
  | y <= x    = Cons y (Cons x xs)
  | otherwise = Cons x (myInsert y xs)
myInsert y Nil = (Cons y Nil)

-- TODO: implement more instances; tests of laws

instance Eq a => Eq (LinkedList a) where
  Nil == Nil = True
  (Cons x xs) == (Cons y ys) = x == y && xs == ys
  _ == _ = False

-- we'll implement "show" a little differently than normal lists, for fun
instance Show a => Show (LinkedList a) where
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs) = show x ++ " - " ++ show xs

instance Monoid (LinkedList a) where
  mempty = Nil
  mappend (Cons x xs) ys = Cons x (mappend xs ys)
  mappend Nil ys = ys
  -- mconcat = foldr mappend mempty -- default implementation is perfectly fine

instance Functor LinkedList where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
instance Foldable LinkedList where
  -- foldMap :: Monoid m => (a -> m) -> LinkedList a -> m
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs
