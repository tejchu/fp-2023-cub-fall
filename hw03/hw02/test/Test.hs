import Test.Tasty

import qualified Test.BankersDeque
import qualified Test.BootstrappedHeap
import qualified Test.SkewBinomialHeap

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Bankers Deque Unit Tests" Test.BankersDeque.unitTests
                , testGroup "Bankers Deque Property Tests" Test.BankersDeque.props
                , testGroup "Bootstrapped Heap Unit Tests" Test.BootstrappedHeap.unitTests
                , testGroup "Bootstrapped Heap Property Tests" Test.BootstrappedHeap.props
                , testGroup "Skew Binomial Heap Unit Tests" Test.SkewBinomialHeap.unitTests
                , testGroup "Skew Binomial Heap Property Tests" Test.SkewBinomialHeap.props
                ]
                )