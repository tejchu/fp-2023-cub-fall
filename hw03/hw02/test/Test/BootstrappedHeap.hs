module Test.BootstrappedHeap where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import BootstrappedHeap
import Prelude hiding (head,tail, last)

testEmptyTests ::  TestTree
testEmptyTests =  testGroup "isEmpty"
    [ testCase "returns True for an empty heap" $ do
        isEmpty (createEmpty :: BootstrappedHeap Int) @?= True
    , testCase "returns False for a non-empty heap" $ do
        let heap = insert 1 (createEmpty :: BootstrappedHeap Int)
        isEmpty heap @?= False
    ]

testFindMinTests ::  TestTree
testFindMinTests =  testGroup "findMin"
    [ testCase "returns the minimum element of the heap" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        findMin heap @?= 1
    ]

testDeleteMinTests ::  TestTree
testDeleteMinTests =  testGroup "deleteMin"
    [ testCase "deletes the minimum element of the heap" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        let heap' = deleteMin heap
        findMin heap' @?= 1
    ]

testInsertTests ::  TestTree
testInsertTests =  testGroup "insert"
    [ testCase "inserts an element into the heap" $ do
        let heap = insert 1 (createEmpty :: BootstrappedHeap Int)
        findMin heap @?= 1
    , testCase "inserts multiple elements into the heap" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        let heap' = insert 0 heap
        findMin heap' @?= 0
    ]

testMergeTests ::  TestTree
testMergeTests = testGroup "merge"
    [ testCase "merges two heaps" $ do
        let heap1 = fromList [3, 1, 4, 1, 5]
        let heap2 = fromList [9, 2, 6, 5]
        let heap' = merge heap1 heap2
        findMin heap' @?= 1
    , testCase "returns the non-empty heap when one heap is empty" $ do
        let heap1 = fromList [3, 1, 4, 1, 5]
        let heap2 = createEmpty :: BootstrappedHeap Int
        let heap' = merge heap1 heap2
        findMin heap' @?= 1
    ]

testFromListTests ::  TestTree
testFromListTests = testGroup "fromList"
    [ testCase "creates a heap from a list of elements" $ do
        let heap = fromList [3, 1, 4, 1, 5, 9, 2, 6, 5]
        findMin heap @?= 1
    ]
  
-- property based testing
prop_insert :: Property
prop_insert = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 100))
  let heap = fromList xs
  let heap' = insert 0 heap
  findMin heap' === 0

prop_merge :: Property
prop_merge = property $ do
  xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.linear 1 100))
  ys <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.linear 1 100))
  let heap1 = fromList xs
  let heap2 = fromList ys
  let heap' = merge heap1 heap2
  findMin heap' === min (findMin heap1) (findMin heap2)

genIntRange :: Gen Int
genIntRange = Gen.int (Range.linear 1 100)

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 20) genIntRange

prop_not_empty :: Property
prop_not_empty = property $ do
  let heap = insert 1 (createEmpty :: BootstrappedHeap Int)
  isEmpty heap === False

prop_empty :: Property
prop_empty = property $ do
  let heap = createEmpty :: BootstrappedHeap Int
  isEmpty heap === True


prop_merge_empty :: Property
prop_merge_empty = property $ do
  let heap1 = createEmpty :: BootstrappedHeap Int
  let heap2 = fromList [1,2,3,4,5]
  let heap' = merge heap1 heap2
  findMin heap' === 1
  
unitTests :: [TestTree]
unitTests = [testEmptyTests
            ,testFindMinTests
            ,testDeleteMinTests
            ,testInsertTests
            ,testMergeTests
            ,testFromListTests
            ]
        
props = [
        testProperty "prop_insert" prop_insert
        , testProperty "prop_empty" prop_empty
        , testProperty "prop_not_empty" prop_not_empty
        , testProperty "prop_merge_empty" prop_merge_empty
        , testProperty "prop_merge" prop_merge
        ]

