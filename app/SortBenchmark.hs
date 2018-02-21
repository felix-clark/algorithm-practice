-- benchmark some sorting algorithms with the criterion package
import Criterion.Main
import Sort
-- import MonadRandom
import Control.Monad.Random

main :: IO ()
main = do
  let descAndFuncs = [("no sort",id)
                     ,("mergesort",mergeSort)
                     ,("quicksort (head-pivot)",quickSort)
                     ,("quicksort (vector conversion to median-of-3)",quickSort')
                     ,("heapsort (max heap)",heapSort)
                     ,("heapsort (min heap)",heapSort')
                     ,("selection sort",selectionSort)
                     ,("insertion sort",insertionSort)
                     ,("bubble sort",bubbleSort)
                     ]
  -- let randEightUnEv = take 8 getRandoms
  -- randEight <- evalRandIO getRandoms :: [Int]
  let singleton = [0] :: [Int]
  let sorted10k = [1..8192] :: [Int]
  let reverse1k = reverse [1..1024] :: [Int]
  -- nf: normal form - forces evaluation ahead of time.
  -- whnf: weak head normal form - doesn't seem to give realistic benchmarks for sorts, e.g. it say selection sort is very fast
  -- it might be instructive in some cases to check both
  defaultMain [ bgroup "singleton" [ bench desc $ nf sf singleton | (desc,sf) <- descAndFuncs ]
              , bgroup "sorted 10k" [ bench desc $ nf sf sorted10k | (desc,sf) <- descAndFuncs ]
              , bgroup "reverse 1k" [ bench desc $ nf sf reverse1k | (desc,sf) <- descAndFuncs ]
              ]
  
