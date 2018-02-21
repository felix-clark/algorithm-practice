module SortSpec (spec) where

import Test.Hspec
import Sort

-- GHC doesn't yet support "impredicative polymorphism" so we have to specify the type of the functions
funcsToTest :: [[Integer] -> [Integer]]
funcsToTest = [mergeSort
              ,quickSort
              ,quickSort'
              ,heapSort
              ,heapSort'
              ,selectionSort
              ,insertionSort
              ,bubbleSort
              ]
  
testLists = [ []
            ,[0]
            ,[3,5,4,6,-5,7,5,1,-3,7]
            ,[-1,4,3,2,-7,9,3]
            ,[4,8,-2,12,9,-4,6]
            ,[-3,2,-7,1,8,3,-2]
            ,[0..12]
            ,[5,4..(-4)]
            ]

spec :: Spec
spec = do
  describe "sorting algorithms" $ do
    it "succeeds if sorting algorithms do not change the size of the input list" $
      and [length input == (length . f) input | input <- testLists, f <- funcsToTest] `shouldBe` True
    it "succeeds if all the sorting algorithms return properly sorted lists" $
      all isSorted sortedLists `shouldBe` True where
      sortedLists = funcsToTest <*> testLists

  -- print sortedLists
  
