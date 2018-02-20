module BinarySearchTree where

-- binary search tree
data Tree a = Nil
            | Node a (Tree a) (Tree a)
            deriving (Eq, Show, Read)


-- filling a BST with a list then flattening it is essentially performing a quicksort
toList :: Tree a -> [a]
toList (Node x ls rs) = toList ls ++ x : toList rs
toList Nil = []

fromList :: Ord a => [a] -> Tree a
-- fromList (x:xs) = insert x (fromList xs)
-- fromList [] = Nil
fromList = foldr insert Nil

-- insert an element into the BST, maintaining sorted structure.
-- O(log n)
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Nil = Node x Nil Nil
insert x (Node y ls rs)
  | x <= y   = Node y (insert x ls) rs
  | x > y    = Node y ls (insert x rs)

