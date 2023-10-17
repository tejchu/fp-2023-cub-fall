module Test.BankersDeque where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import BankersDeque
import Prelude hiding (head,tail, last)

testEmptyTests ::  TestTree
testEmptyTests = testGroup "isEmpty"
    [ testCase "returns True for an empty deque" $ do
        isEmpty (createEmpty :: BankersDeque Int) @?= True
    , testCase "returns False for a non-empty deque" $ do
        let deque = cons 1 (createEmpty :: BankersDeque Int)
        isEmpty deque @?= False
    ]

testHeadTests ::  TestTree
testHeadTests = testGroup "head"
    [ testCase "returns the first element of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        head deque @?= 1
    ]

testTailTests ::  TestTree
testTailTests = testGroup "tail"
    [ testCase "removes the first element of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        let deque' = tail deque
        head deque' @?= 2
        last deque' @?= 3
    ]

testLastTests ::  TestTree
testLastTests = testGroup "cons"
    [ testCase "adds an element to the front of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        head deque @?= 1
        last deque @?= 3
    , testCase "adds multiple elements to the front of the deque" $ do
        let deque = cons 1 $ cons 2 $ cons 3 (createEmpty :: BankersDeque Int)
        let deque' = cons 4 $ cons 5 deque
        head deque' @?= 4
        last deque' @?= 3
    ]

testInitTests ::  TestTree
testInitTests = testGroup "enqueue"
    [ testCase "adds an element to the end of the deque" $ do
        let deque = enqueue (createEmpty :: BankersDeque Int) 1
        last deque @?= 1
    , testCase "adds multiple elements to the end of the deque" $ do
        let deque = enqueue (enqueue (enqueue (createEmpty :: BankersDeque Int) 1) 2) 3
        head deque @?= 1
        last deque @?= 3
    ]

-- Property-based testing

prop_empty :: Property
prop_empty = property $ do
    let deque = createEmpty :: BankersDeque Int
    isEmpty deque === True

prop_cons :: Property
prop_cons = property $ do
    x <- forAll $ Gen.int (Range.linear 0 100)
    let deque = createEmpty :: BankersDeque Int
    let deque' = cons x deque
    isEmpty deque' === False
    head deque' === x

prop_enqueue :: Property
prop_enqueue = property $ do
    x <- forAll $ Gen.int (Range.linear 0 100)
    let deque = createEmpty :: BankersDeque Int
    let deque' = enqueue deque x
    isEmpty deque' === False
    last deque' === x


unitTests :: [TestTree]
unitTests = [testEmptyTests
            ,testHeadTests
            ,testTailTests
            ,testLastTests
            ,testInitTests
            ]

props = [testProperty "isEmpty" prop_empty
        ,testProperty "cons" prop_cons
        ,testProperty "enqueue" prop_enqueue]