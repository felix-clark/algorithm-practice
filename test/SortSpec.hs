module SortSpec where

import Sort
import Test.Hspec

-- main :: IO ()
-- main = hspec $ do
spec :: Spec
spec = do
  describe "sort algorithms" $ do
    it "succeeds if all the sorting algorithms properly sort all of the testing lists" $
      all isSorted sortedLists `shouldBe` True where
      -- GHC doesn't yet support "impredicative polymorphism" so we have to specify the type of the functions
      funcsToTest = [mergeSort,
                     quickSort,
                     selectionSort,
                     insertionSort,
                     bubbleSort] :: [[Integer] -> [Integer]]
      testLists = [ [],
                    [0],
                    [3,5,4,6,-5,7,5,1,-3,7],
                    [-1,4,3,2,-7,9,3],
                    [4,8,-2,12,9,-4,6],
                    [-3,2,-7,1,8,3,-2],
                    [0..12],
                    [5,4..(-4)]
                  ]
      sortedLists = funcsToTest <*> testLists

  -- print sortedLists
  
  
  -- TODO: generate multiple random long lists and test runtime
