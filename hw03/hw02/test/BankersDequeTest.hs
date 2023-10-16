module BankersDequeTest (spec) where
import Test.Hspec
import BankersDeque
import Prelude hiding (head, last, tail, init)


spec :: Spec 

spec = do
  describe "Banker's Deque Tests" $ do
    describe "isEmpty" $ do
      it "returns True for an empty deque" $ do
        isEmpty (createEmpty :: BankersDeque Int) `shouldBe` True

      it "returns False for a non-empty deque" $ do
        let deque = cons 1 (createEmpty :: BankersDeque Int)
        isEmpty deque `shouldBe` False

    describe "head" $ do
      it "returns the first element of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        head deque `shouldBe` 1

    describe "tail" $ do
      it "removes the first element of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        let deque' = tail deque
        head deque' `shouldBe` 2
        last deque' `shouldBe` 3

    describe "cons" $ do
      it "adds an element to the front of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        head deque `shouldBe` 1
        last deque `shouldBe` 3

      it "adds multiple elements to the front of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        let deque' = cons 4 $ cons 5 deque
        head deque' `shouldBe` 4
        last deque' `shouldBe` 3

    describe "enqueue" $ do
      it "adds an element to the end of the deque" $ do
        let deque = enqueue (createEmpty :: BankersDeque Int) 1
        last deque `shouldBe` 1

      it "adds multiple elements to the end of the deque" $ do
        let deque = enqueue (enqueue (enqueue (createEmpty :: BankersDeque Int) 1) 2) 3
        head deque `shouldBe` 1
        last deque `shouldBe` 3

    describe "init" $ do
      it "removes the last element of the deque" $ do
        let deque = enqueue (enqueue (enqueue (createEmpty :: BankersDeque Int) 1) 2) 3
        let deque' = init deque
        head deque' `shouldBe` 1
        last deque' `shouldBe` 2

    describe "last" $ do
      it "returns the last element of the deque" $ do
        let deque = enqueue (enqueue (enqueue (createEmpty :: BankersDeque Int) 1) 2) 3
        last deque `shouldBe` 3
