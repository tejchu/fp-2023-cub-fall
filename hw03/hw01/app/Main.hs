module Main where

import Criterion.Main
import Test.Hspec
import BankersQueueTest
import PureQueueTest
import RealTimeQueueTest
import PhysicistQueueTest
import TreeTest1
import TreeTest2
import TreeTest3

import Queue
import BankersQueue
import PureQueue
import RealTimeQueue
import PhysicistQueue
import Prelude hiding (head,tail)


main :: IO ()
main = hspec $ do 
    BankersQueueTest.spec
    PureQueueTest.spec
    RealTimeQueueTest.spec
    PhysicistQueueTest.spec
    TreeTest1.spec
    TreeTest2.spec
    TreeTest3.spec
    
