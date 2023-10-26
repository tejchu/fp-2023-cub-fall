module Test.PureQueue where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import PureQueue
import Prelude hiding (head,tail)


isEmptyTests :: TestTree
isEmptyTests = testGroup "isEmpty"
  [ testCase "returns True for an empty queue" $ do
      isEmpty (createEmpty :: PureQueue Int) @?= True

  , testCase "returns False for a non-empty queue" $ do
      let queue = enqueue (createEmpty :: PureQueue Int) 1
      isEmpty queue @?= False
  ]

enqueueTests :: TestTree
enqueueTests = testGroup "enqueue"
  [ testCase "adds an element to the end of the queue" $ do
      head (enqueue (createEmpty :: PureQueue Int) 1) @?= 1

  , testCase "adds multiple elements to the end of the queue" $ do
      head (enqueue (enqueue (enqueue (createEmpty :: PureQueue Int) 1) 2) 3) @?= 1
  ]

headTests :: TestTree
headTests = testGroup "head"
  [ testCase "returns the first element of the queue" $ do
      head (enqueue (enqueue (enqueue (createEmpty :: PureQueue Int) 1) 2) 3) @?= 1
  ]

tailTests :: TestTree
tailTests = testGroup "tail"
  [ testCase "removes the first element of the queue" $ do
      let queue = enqueue (enqueue (enqueue (createEmpty :: PureQueue Int) 1) 2) 3
      let queue' = tail queue
      head queue' @?= 2
  ]

-- Property-based tests for this queue now moved to PropertyBasedTests.hs

unitTests :: [TestTree]
unitTests = [isEmptyTests
  , enqueueTests
  , headTests
  , tailTests]

