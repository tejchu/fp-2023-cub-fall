module Test.Tree1 where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import Tree1

testMember :: TestTree
testMember = testGroup "member"
    [ testCase "returns False for an empty tree" $ do
        member 1 (E :: Tree Int) @?= False
    , testCase "returns True for a single element tree" $ do
        member 1 (insert 1 (E :: Tree Int)) @?= True
    , testCase "returns False for a non-existing member" $ do
        member 2 (insert 1 (E :: Tree Int)) @?= False
    , testCase "returns True for a multiple element tree" $ do
        member 2 (insert 1 (insert 2 (E :: Tree Int))) @?= True
    ]

testInsert :: TestTree
testInsert = testGroup "insert"
    [ testCase "inserts an element into an empty tree" $ do
        let tree = insert 1 (E :: Tree Int)
        treeToString tree @?= "(B E 1 E)"
    , testCase "inserts an element into a non-empty tree" $ do
        let tree = insert 2 (insert 1 (E :: Tree Int))
        treeToString tree @?= "(B E 1 (R E 2 E))"
    , testCase "inserts an element into a non-empty tree and maintains red-black properties" $ do
        let tree = insert 3 (insert 2 (insert 1 (E :: Tree Int)))
        treeToString tree @?= "(B (B E 1 E) 2 (B E 3 E))"
    ]

testDelete :: TestTree
testDelete = testGroup "delete"
  [ testCase "deletes an element from a tree with a single element" $ do
      let tree = insert 1 (E :: Tree Int)
      let result = delete 1 tree
      result @?= (E :: Tree Int)

    , testCase "deletes an element from a tree with multiple elements" $ do
      let tree = insert 1 (insert 2 (insert 3 (E :: Tree Int)))
      let result = delete 3 tree
      treeToString result @?= "(B (R E 1 E) 2 E)"
    ]

testBalance :: TestTree
testBalance = testGroup "balance"
  [ testCase "balances a red-red violation in a left-left scenario" $ do
      let tree = T R (T R (T B E 1 E) 2 (T B E 3 E)) 4 E
      let result = balance B (T R (T R (T B E 1 E) 2 (T B E 3 E)) 4 E) 5 E
      treeToString result @?= "(R (B (B E 1 E) 2 (B E 3 E)) 4 (B E 5 E))"

    , testCase "balances a red-red violation in a right-left scenario" $ do
      let tree = T R (T R (T B E 1 E) 2 (T B E 4 E)) 3 (T B E 5 E)
      let result = balance B (T R (T R (T B E 1 E) 2 (T B E 4 E)) 3 (T B E 5 E)) 6 E
      treeToString result @?= "(R (B (B E 1 E) 2 (B E 4 E)) 3 (B (B E 5 E) 6 E))"
    ]

-- Utility function to check if the root of the tree is black
prop_blackRoot ::  Property
prop_blackRoot = property $ do
    let tree = insert 1 (E :: Tree Int)
    let result = tree
    case result of
      T B _ _ _ -> success
      _ -> failure

unitTests :: [TestTree]
unitTests = [testMember, testInsert, testDelete, testBalance]

props = [testProperty "black root" prop_blackRoot]