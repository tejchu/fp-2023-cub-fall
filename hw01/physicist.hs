import Criterion.Main

data PhysicistsQueue a = PQ {
    wList    :: [a],
    fList    :: [a],
    lenF     :: Int,
    rList    :: [a],
    lenR     :: Int
} deriving (Show)

createEmptyQueue :: PhysicistsQueue a
createEmptyQueue = PQ [] [] 0 [] 0

isQueueEmpty :: PhysicistsQueue a -> Bool
isQueueEmpty (PQ [] _ _ _ _) = True
isQueueEmpty _               = False

getFrontElement :: PhysicistsQueue a -> Maybe a
getFrontElement (PQ [] _ _ _ _)      = Nothing
getFrontElement (PQ (x:_) _ _ _ _)   = Just x

enqueueElement :: a -> PhysicistsQueue a -> PhysicistsQueue a
enqueueElement x (PQ w f lenF r lenR) = maintainQueueInvariant $ PQ w f lenF (x:r) (lenR + 1)

dequeueElement :: PhysicistsQueue a -> Maybe (a, PhysicistsQueue a)
dequeueElement (PQ [] _ _ _ _)       = Nothing
dequeueElement (PQ (x:w) f lenF r lenR) = Just (x, maintainQueueInvariant $ PQ w (tail f) (lenF - 1) r lenR)

maintainQueueInvariant :: PhysicistsQueue a -> PhysicistsQueue a
maintainQueueInvariant q@(PQ [] f lenF r lenR) = 
    let w' = evaluateList f
    in PQ w' f lenF r lenR
maintainQueueInvariant q@(PQ w f lenF r lenR)
    | lenR <= lenF = q
    | otherwise    = 
        let w' = evaluateList f
        in PQ w' (w' ++ reverse r) (lenF + lenR) [] 0

evaluateList :: [a] -> [a]
evaluateList xs = foldr seq xs xs


main :: IO ()
main = defaultMain
  [ bench "enqueue" $ whnf (benchmarkEnqueue 10000) emptyQueue
  , bench "dequeue" $ whnf (benchmarkDequeue 10000) filledQueue
  , bench "front" $ whnf benchmarkFront filledQueue
  ]

-- sample queues
sampleQueue :: PhysicistsQueue Int
sampleQueue = foldr enqueueElement emptyQueue [1..10000]

emptyQueue :: PhysicistsQueue Int
emptyQueue = createEmptyQueue

filledQueue :: PhysicistsQueue Int
filledQueue = sampleQueue

benchmarkEnqueue :: Int -> PhysicistsQueue Int -> PhysicistsQueue Int
benchmarkEnqueue n q = foldr enqueueElement q [1..n]

benchmarkDequeue :: Int -> PhysicistsQueue Int -> PhysicistsQueue Int
benchmarkDequeue n q = go n q
  where
    go 0 queue = queue
    go k queue = case dequeueElement queue of
      Just (_, newQueue) -> go (k - 1) newQueue
      Nothing            -> emptyQueue

benchmarkFront :: PhysicistsQueue Int -> Maybe Int
benchmarkFront q = getFrontElement q
