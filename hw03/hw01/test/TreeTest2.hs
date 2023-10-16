module TreeTest2 (spec) where

import Test.Hspec
import Tree2

spec :: Spec
spec = do
  describe "insert" $ do
    it "adds an element to an empty tree" $ do
      let tree = insert 1 (E :: Tree Int)
      treeToString tree `shouldBe` "(B E 1 E)"

    it "adds an element to a non-empty tree" $ do
      let tree = insert 2 (insert 1 (E :: Tree Int))
      treeToString tree `shouldBe` "(B E 1 (R E 2 E))"

    it "maintains the red-black tree properties" $ do
      let tree = insert 3 (insert 2 (insert 1 (E :: Tree Int)))
      treeToString tree `shouldBe` "(B (B E 1 E) 2 (B E 3 E))"

    it "maintains the red-black tree properties for larger trees" $ do
      let tree = insert 10 (insert 7 (insert 5 (insert 3 (insert 1 (E :: Tree Int))))) 
      treeToString tree `shouldBe` "(B (B E 1 E) 3 (R (B E 5 E) 7 (B E 10 E)))"