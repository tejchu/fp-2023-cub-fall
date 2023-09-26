
import Criterion.Main

data BankersQueue a = BQ {
    frontList :: [a],
    rearList  :: [a]
} deriving (Show)

createEmptyQueue :: BankersQueue a
createEmptyQueue = BQ [] []

isQueueEmpty :: BankersQueue a -> Bool
isQueueEmpty (BQ [] []) = True
isQueueEmpty _          = False

getFrontElement :: BankersQueue a -> Maybe a
getFrontElement (BQ [] [])      = Nothing
getFrontElement (BQ (x:_) _)    = Just x

enqueueElement :: a -> BankersQueue a -> BankersQueue a
enqueueElement x (BQ front rear) = maintainQueueInvariant $ BQ front (x:rear)

dequeueElement :: BankersQueue a -> Maybe (a, BankersQueue a)
dequeueElement (BQ [] [])       = Nothing
dequeueElement (BQ (x:front) rear) = Just (x, maintainQueueInvariant $ BQ front rear)


maintainQueueInvariant :: BankersQueue a -> BankersQueue a
maintainQueueInvariant q@(BQ front rear)
    | length front >= length rear = q
    | otherwise                   = BQ (front ++ reverse rear) []

 
main :: IO ()
main = defaultMain
  [ bench "enqueue" $ whnf (benchmarkEnqueue 10000) emptyQueue
  , bench "dequeue" $ whnf (benchmarkDequeue 10000) filledQueue
  , bench "front" $ whnf benchmarkFront filledQueue
  ]

-- Create a sample queue with 10000 elements
sampleQueue :: BankersQueue Int
sampleQueue = foldr enqueueElement emptyQueue [1..10000]

emptyQueue :: BankersQueue Int
emptyQueue = createEmptyQueue

filledQueue :: BankersQueue Int
filledQueue = sampleQueue

benchmarkEnqueue :: Int -> BankersQueue Int -> BankersQueue Int
benchmarkEnqueue n q = foldr enqueueElement q [1..n]

benchmarkDequeue :: Int -> BankersQueue Int -> BankersQueue Int
benchmarkDequeue n q = go n q
  where
    go 0 queue = queue
    go k queue = case dequeueElement queue of
      Just (_, newQueue) -> go (k - 1) newQueue
      Nothing            -> emptyQueue

benchmarkFront :: BankersQueue Int -> Maybe Int
benchmarkFront q = getFrontElement q