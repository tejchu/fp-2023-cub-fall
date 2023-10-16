module TreeTest3 (spec) where

import Test.Hspec
import Tree3

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

    it "does not change the tree if the element is already in the tree" $ do
      let tree = insert 1 (E :: Tree Int)
      let newTree = insert 1 tree
      treeToString newTree `shouldBe` treeToString tree

    it "adds multiple elements to the tree" $ do
      let tree = insert 3 (insert 2 (insert 1 (E :: Tree Int)))
      let newTree = insert 4 (insert 5 tree)
      treeToString newTree `shouldBe` "(B (B E 1 E) 2 (R (B E 3 E) 4 (B E 5 E)))"