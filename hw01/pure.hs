import Criterion.Main

data Queue a = Queue [a] [a] deriving (Show)

createEmptyQueue :: Queue a
createEmptyQueue = Queue [] []

enqueueElement :: a -> Queue a -> Queue a
enqueueElement x (Queue f r) = Queue f (x:r)

head :: Queue a -> Maybe a
head (Queue [] _) = Nothing
head (Queue (x:_) _) = Just x

tail :: Queue a -> Queue a
tail (Queue [] _) = createEmptyQueue
tail (Queue (_:f) r) = Queue f r

main :: IO ()
main = defaultMain
  [ bench "enqueue" $ whnf (benchmarkEnqueue 10000) emptyQueue
  ]

-- Create a sample queue with 10000 elements
sampleQueue :: Queue Int
sampleQueue = foldr enqueueElement createEmptyQueue [1..10000]

emptyQueue :: Queue Int
emptyQueue = createEmptyQueue

filledQueue :: Queue Int
filledQueue = sampleQueue

benchmarkEnqueue :: Int -> Queue Int -> Queue Int
benchmarkEnqueue n q = foldr enqueueElement q [1..n]

