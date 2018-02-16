module LinkedListSpec where

import Test.Hspec
import LinkedList

spec :: Spec
spec = do
  describe "linked list" $ do
    it "conversion to and from the built-in list" $
      (Cons 1 (Cons 5 (Cons (-1) Nil))) `shouldBe` (myFromList [1,5,-1])
      
