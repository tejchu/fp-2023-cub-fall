import Criterion.Main

data RealTimeQueue a = RTQ {
    frontList    :: [a], 
    reversedList :: [a],  
    scheduleList :: [a]   
} deriving (Show)

createEmptyQueue :: RealTimeQueue a
createEmptyQueue = RTQ [] [] []

isQueueEmpty :: RealTimeQueue a -> Bool
isQueueEmpty (RTQ [] _ _) = True
isQueueEmpty _            = False

rotateList :: [a] -> [a] -> [a] -> [a]
rotateList [] [y] a = y:a
rotateList (x:xs) (y:ys) a = x : rotateList xs ys (y:a)

maintainQueueInvariant :: RealTimeQueue a -> RealTimeQueue a
maintainQueueInvariant (RTQ f r (_:s)) = RTQ f r s
maintainQueueInvariant (RTQ f r []) = 
    let f' = rotateList f r []
    in RTQ f' [] f'

getFrontElement :: RealTimeQueue a -> Maybe a
getFrontElement (RTQ [] _ _)      = Nothing
getFrontElement (RTQ (x:_) _ _)   = Just x

enqueueElement :: a -> RealTimeQueue a -> RealTimeQueue a
enqueueElement x q@(RTQ f r s) = maintainQueueInvariant $ RTQ f (x:r) s

dequeueElement :: RealTimeQueue a -> Maybe (a, RealTimeQueue a)
dequeueElement (RTQ [] _ _)       = Nothing
dequeueElement (RTQ (x:f) r s)    = Just (x, maintainQueueInvariant $ RTQ f r s)

main :: IO ()
main = defaultMain
  [ bench "enqueue" $ whnf (benchmarkEnqueue 10000) emptyQueue
  , bench "dequeue" $ whnf (benchmarkDequeue 10000) filledQueue
  , bench "front" $ whnf benchmarkFront filledQueue
  ]

-- sample queues
sampleQueue :: RealTimeQueue Int
sampleQueue = foldr enqueueElement emptyQueue [1..10000]

emptyQueue :: RealTimeQueue Int
emptyQueue = createEmptyQueue

filledQueue :: RealTimeQueue Int
filledQueue = sampleQueue

benchmarkEnqueue :: Int -> RealTimeQueue Int -> RealTimeQueue Int
benchmarkEnqueue n q = foldr enqueueElement q [1..n]

benchmarkDequeue :: Int -> RealTimeQueue Int -> RealTimeQueue Int
benchmarkDequeue n q = go n q
  where
    go 0 queue = queue
    go k queue = case dequeueElement queue of
      Just (_, newQueue) -> go (k - 1) newQueue
      Nothing            -> emptyQueue

benchmarkFront :: RealTimeQueue Int -> Maybe Int
benchmarkFront q = getFrontElement q