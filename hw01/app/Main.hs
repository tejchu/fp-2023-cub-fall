module Main where

import Criterion.Main

import Queue
import BankersQueue
import PureQueue
import RealTimeQueue
import PhysicistQueue
import Prelude hiding (head,tail)

filteredNumbers f = filter f [0..100000]::[Int]

main :: IO ()
main = defaultMain [
    bgroup "Queue enqueue" [
        bench "Enqueue in Purely Functional Queue" $ whnf (foldl enqueue (createEmpty::PureQueue Int))  ([0..50000]::[Int]),
        bench "Enqueue in Banker's Queue" $ whnf (foldl enqueue (createEmpty::BankersQueue Int))  ([0..50000]::[Int]),
        bench "Enqueue in Physicist's Queue" $ whnf (foldl enqueue (createEmpty::PhysicistQueue Int))  ([0..50000]::[Int]),
        bench "Enqueue in Real-Time Queue" $ whnf (foldl enqueue (createEmpty::RealTimeQueue Int))  ([0..50000]::[Int])
    ],
    bgroup "Queue dequeue" [
        bench "Dequeue in Purely Functional Queue" $ whnf (foldl (\q x -> tail q) (foldl enqueue (createEmpty::PureQueue Int) ([0..5000]::[Int])))  ([0..5000]::[Int]),
        bench "Dequeue in Banker's Queue" $ whnf (foldl (\q x -> tail q) (foldl enqueue (createEmpty::BankersQueue Int) ([0..5000]::[Int])))  ([0..5000]::[Int]),
        bench "Dequeue in Physicist's Queue" $ whnf (foldl (\q x -> tail q) (foldl enqueue (createEmpty::PhysicistQueue Int) ([0..5000]::[Int])))  ([0..5000]::[Int]),
        bench "Dequeue in Real-Time Queue" $ whnf (foldl (\q x -> tail q) (foldl enqueue (createEmpty::RealTimeQueue Int) ([0..5000]::[Int])))  ([0..5000]::[Int])
    ]
    ]