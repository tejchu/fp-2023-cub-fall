module Test.PropertyBasedTests where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import RealTimeQueue
import BankersQueue
import PureQueue
import PhysicistQueue
import Prelude hiding (head,tail)

-- Property-based testing

genIntRange :: Gen Int
genIntRange = Gen.int (Range.linear 1 100)

genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 20) genIntRange

prop_enqueue :: Queue q => q Int -> Property
prop_enqueue q = property $ do
  x <- forAll $ genIntRange
  y <- forAll $ genIntRange
  let q' = enqueue (enqueue q x) y
  let z = head q'
  z === x

prop_tail :: Queue q => q Int -> Property
prop_tail q = property $ do
  x <- forAll $ genIntRange
  y <- forAll $ genIntRange
  let q' = enqueue (enqueue q x) y
  let q'' = tail q'
  let z = head q''
  z === y

prop_isEmpty :: Queue q => q Int -> Property
prop_isEmpty q = property $ do
  xs <- forAll genIntList
  let q' = foldl enqueue q xs
  let isEmptyResult = isEmpty q'
  if null xs 
      then assert $ isEmptyResult
      else assert $ not $ isEmptyResult


rtq = [testProperty "preserved order when enqueuing - test on head" $ prop_enqueue (createEmpty :: RealTimeQueue Int)
    , testProperty "preserved order when enqueuing - test on tail" $ prop_tail (createEmpty :: RealTimeQueue Int)
    , testProperty "if elements are enqueued, queue must not be empty" $ prop_isEmpty (createEmpty :: RealTimeQueue Int) 
    ] 

bq = [testProperty "preserved order when enqueuing - test on head" $ prop_enqueue (createEmpty :: BankersQueue Int)
    , testProperty "preserved order when enqueuing - test on tail" $ prop_tail (createEmpty :: BankersQueue Int)
    , testProperty "if elements are enqueued, queue must not be empty" $ prop_isEmpty (createEmpty :: BankersQueue Int) 
    ]

pq = [testProperty "preserved order when enqueuing - test on head" $ prop_enqueue (createEmpty :: PureQueue Int)
    , testProperty "preserved order when enqueuing - test on tail" $ prop_tail (createEmpty :: PureQueue Int)
    , testProperty "if elements are enqueued, queue must not be empty" $ prop_isEmpty (createEmpty :: PureQueue Int) 
    ]

phq = [testProperty "preserved order when enqueuing - test on head" $ prop_enqueue (createEmpty :: PhysicistQueue Int)
    , testProperty "preserved order when enqueuing - test on tail" $ prop_tail (createEmpty :: PhysicistQueue Int)
    , testProperty "if elements are enqueued, queue must not be empty" $ prop_isEmpty (createEmpty :: PhysicistQueue Int) 
    ]
