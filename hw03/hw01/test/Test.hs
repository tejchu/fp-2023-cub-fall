import Test.Tasty

import qualified Test.BankersQueue
import qualified Test.PhysicistQueue
import qualified Test.PureQueue
import qualified Test.RealTimeQueue
import qualified Test.Tree1
import qualified Test.Tree2
import qualified Test.Tree3

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Bankers Queue Unit Tests" Test.BankersQueue.unitTests
                    , testGroup "ReverseProp" Test.BankersQueue.props
                   , testGroup "Physicist Queue Unit Tests" Test.PhysicistQueue.unitTests
                    , testGroup "Physicist Queue Property Tests" Test.PhysicistQueue.props
                    , testGroup "Pure Queue Unit Tests" Test.PureQueue.unitTests
                    , testGroup "Pure Queue Property Tests" Test.PureQueue.props
                    , testGroup "RealTime Queue Unit Tests" Test.RealTimeQueue.unitTests
                    , testGroup "RealTime Queue Property Tests" Test.RealTimeQueue.props
                    , testGroup "Tree1 Unit Tests" Test.Tree1.unitTests
                    , testGroup "Tree1 Property Tests" Test.Tree1.props
                    , testGroup "Tree2 Unit Tests" Test.Tree2.unitTests
                    -- , testGroup "Tree2 Property Tests" Test.Tree2.props
                    , testGroup "Tree3 Unit Tests" Test.Tree3.unitTests
                    -- , testGroup "Tree3 Property Tests" Test.Tree3.props
                ])