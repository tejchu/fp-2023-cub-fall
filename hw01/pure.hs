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
