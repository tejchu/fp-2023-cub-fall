module SkewBinomialHeapTest (spec) where
import Test.Hspec
import SkewBinomialHeap


spec :: Spec
spec = do
  describe "SkewBinomialHeap tests" $ do
    it "Insert" $ do
      let heap = insert 3 (insert 2 (insert 1 createEmpty))
      findMin heap `shouldBe` 1

    it "InsertAll" $ do
      let heap = insertAll [3, 2, 1] createEmpty
      findMin heap `shouldBe` 1

    it "DeleteMin" $ do
      let heap = insertAll [3, 2, 1] createEmpty
      let heap' = deleteMin heap
      findMin heap' `shouldBe` 2

    it "Merge" $ do
      let heap1 = insertAll [3, 1] createEmpty
      let heap2 = insertAll [4, 2] createEmpty
      let heap = merge heap1 heap2
      findMin heap `shouldBe` 1

    it "IsEmpty" $ do
      let heap = createEmpty :: SkewBinomialHeap Int
      isEmpty heap `shouldBe` True
      let heap' = insert 1 heap
      isEmpty heap' `shouldBe` False
