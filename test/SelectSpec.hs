module SelectSpec where

import Test.Hspec
import Select
import Sort

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "selection algorithms" $ do
    it "compares selection algorithms to result of indexing off a fully-sorted list" $
      -- should extend these tests significantly but for now let's just have one that works
      quickSelect 4 ls `shouldBe` (quickSort ls) !! 4 where
      ls = [3,1,-2,5,7,-4,2]
