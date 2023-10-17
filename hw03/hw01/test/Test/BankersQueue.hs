module Test.BankersQueue where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import BankersQueue
import Prelude hiding (head,tail)


isEmptyTests :: TestTree
isEmptyTests = testGroup "isEmpty"
  [ testCase "returns True for an empty queue" $
      isEmpty (createEmpty :: BankersQueue Int) @?= True

  , testCase "returns False for a non-empty queue" $ do
      let queue = enqueue (createEmpty :: BankersQueue Int) 1
      isEmpty queue @?= False
  ]

enqueueTests :: TestTree
enqueueTests = testGroup "enqueue"
  [ testCase "adds an element to the end of the queue" $ do
      let queue = enqueue (createEmpty :: BankersQueue Int) 1
      head queue @?= 1

  , testCase "adds multiple elements to the end of the queue" $ do
      let queue = enqueue (enqueue (enqueue (createEmpty :: BankersQueue Int) 1) 2) 3
      head queue @?= 1
  ]

headTests :: TestTree
headTests = testGroup "head"
  [ testCase "returns the first element of the queue" $ do
      let queue = enqueue (enqueue (enqueue (createEmpty :: BankersQueue Int) 1) 2) 3
      head queue @?= 1
  ]

tailTests :: TestTree
tailTests = testGroup "tail"
  [ testCase "removes the first element of the queue" $ do
      let queue = enqueue (enqueue (enqueue (createEmpty :: BankersQueue Int) 1) 2) 3
      let queue' = tail queue
      head queue' @?= 2
  ]

-- Property-based testing
genIntRange :: Gen Int
genIntRange = Gen.int (Range.linear 1 100)

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 20) genIntRange

prop_enqueue :: Property
prop_enqueue = property $ do
  x <- forAll $ genIntRange
  y <- forAll $ genIntRange
  let q = createEmpty :: BankersQueue Int
  let q' = enqueue (enqueue q x) y
  let z = head q'
  z === x

prop_tail :: Property
prop_tail = property $ do
  x <- forAll $ genIntRange
  y <- forAll $ genIntRange
  let q = createEmpty :: BankersQueue Int
  let q' = enqueue (enqueue q x) y
  let q'' = tail q'
  let z = head q''
  z === y

prop_isEmpty :: Property
prop_isEmpty = property $ do
  xs <- forAll genIntList
  let queue = createEmpty :: BankersQueue Int
  let q = foldl enqueue queue xs
  let isEmptyResult = isEmpty q
  if null xs 
      then assert $ isEmptyResult
      else assert $ not $ isEmptyResult


unitTests :: [TestTree]
unitTests = [isEmptyTests
  , enqueueTests
  , headTests
  , tailTests]


props = [
    testProperty "prop_enqueue" prop_enqueue,
    testProperty "prop_tail" prop_tail,
    testProperty "prop_isEmpty" prop_isEmpty]
