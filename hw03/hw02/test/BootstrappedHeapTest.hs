module BootstrappedHeapTest (spec) where

import Test.Hspec
import BootstrappedHeap

spec :: Spec
spec = do
  describe "BootstrappedHeap Tests" $ do
    describe "isEmpty" $ do
      it "returns True for an empty heap" $ do
        isEmpty (createEmpty :: BootstrappedHeap Int) `shouldBe` True

      it "returns False for a non-empty heap" $ do
        let heap = insert 1 (createEmpty :: BootstrappedHeap Int)
        isEmpty heap `shouldBe` False

    describe "findMin" $ do
      it "returns the minimum element of the heap" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        findMin heap `shouldBe` 1

    describe "deleteMin" $ do
      it "deletes the minimum element of the heap" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        let heap' = deleteMin heap
        findMin heap' `shouldBe` 1


    describe "insert" $ do
      it "inserts an element into the heap" $ do
        let heap = insert 1 (createEmpty :: BootstrappedHeap Int)
        findMin heap `shouldBe` 1

      it "inserts multiple elements into the heap" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        let heap' = insert 0 heap
        findMin heap' `shouldBe` 0

    describe "merge" $ do
      it "merges two heaps" $ do
        let heap1 = fromList [3, 1, 4, 1, 5]
        let heap2 = fromList [9, 2, 6, 5]
        let heap' = merge heap1 heap2
        findMin heap' `shouldBe` 1

      it "returns the non-empty heap when one heap is empty" $ do
        let heap1 = fromList [3, 1, 4, 1, 5]
        let heap2 = createEmpty :: BootstrappedHeap Int
        let heap' = merge heap1 heap2
        findMin heap' `shouldBe` 1

    describe "fromList" $ do
      it "creates a heap from a list of elements" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        findMin heap `shouldBe` 1