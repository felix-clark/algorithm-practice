--- if this package grows significantly, it will be useful to separate tests by modules.
-- see https://stackoverflow.com/questions/43263965/how-to-run-multiple-test-files-with-haskell-stack-project
import Sort
import Test.Hspec

main :: IO ()
main = do

  -- GHC doesn't yet support "impredicative polymorphism" so we have to specify the type of the functions
  let funcsToTest = [mergeSort, quickSort, insertionSort] :: [[Integer] -> [Integer]]
  -- let funcsToTest = [mergeSort, quickSort, bubbleSort] :: [[Integer] -> [Integer]]

  let testLists = [ [],
                    [0],
                    [3,5,4,6,-5,7,5,1,-3,7],
                    [-1,4,3,2,-7,9,3],
                    [4,8,-2,12,9,-4,6],
                    [-3,2,-7,1,8,3,-2],
                    [0..12],
                    [5,4..(-4)]
                  ]

  let sortedLists = funcsToTest <*> testLists

  -- print sortedLists
  
  all isSorted sortedLists `shouldBe` True
  
  -- TODO: generate multiple random long lists and test runtime
