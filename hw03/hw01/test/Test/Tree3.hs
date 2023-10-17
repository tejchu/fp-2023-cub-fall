module Test.Tree3 where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import Tree3

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



unitTests :: [TestTree]
unitTests = [testMember, testInsert]