module TreeTest1 (spec) where

import Test.Hspec
import Tree1

spec :: Spec
spec = do
  describe "member" $ do
    it "returns False for an empty tree" $ do
      member 1 (E :: Tree Int) `shouldBe` False

    it "returns True for a tree with a single element" $ do
      let tree = insert 1 (E :: Tree Int)
      member 1 tree `shouldBe` True

    it "returns False for a tree without the searched element" $ do
      let tree = insert 1 (E :: Tree Int)
      member 2 tree `shouldBe` False

    it "returns True for a tree with multiple elements" $ do
      let tree = insert 1 (insert 2 (insert 3 (E :: Tree Int)))
      member 2 tree `shouldBe` True

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