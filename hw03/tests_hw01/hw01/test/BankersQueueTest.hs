module BankersQueueTest (spec) where
import Prelude hiding (head,tail)

import Test.Hspec
import BankersQueue 

spec :: Spec
spec = do
  describe "isEmpty" $ do
    it "returns True for an empty queue" $ do
      isEmpty (createEmpty :: BankersQueue Int) `shouldBe` True

    it "returns False for a non-empty queue" $ do
      let queue = enqueue (createEmpty :: BankersQueue Int) 1
      isEmpty queue `shouldBe` False

  describe "enqueue" $ do
    it "adds an element to the end of the queue" $ do
      let queue = enqueue (createEmpty :: BankersQueue Int) 1
      head queue `shouldBe` 1

    it "adds multiple elements to the end of the queue" $ do
      let queue = enqueue (enqueue (enqueue (createEmpty :: BankersQueue Int) 1) 2) 3
      head queue `shouldBe` 1

  describe "head" $ do
    it "returns the first element of the queue" $ do
      head (enqueue (enqueue (enqueue (createEmpty :: BankersQueue Int) 1) 2) 3) `shouldBe` 1


  describe "tail" $ do
    it "removes the first element of the queue" $ do
      let queue = enqueue (enqueue (enqueue (createEmpty :: BankersQueue Int) 1) 2) 3
      let queue' = tail queue
      head queue' `shouldBe` 2