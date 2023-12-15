data RealTimeDeque a = RTDeque [a] [a]

emptyDeque :: RealTimeDeque a
emptyDeque = RTDeque [] []

isEmpty :: RealTimeDeque a -> Bool
isEmpty (RTDeque front back) = null front && null back

enqueueFront :: a -> RealTimeDeque a -> RealTimeDeque a
enqueueFront x (RTDeque front back) = RTDeque (x : front) back

enqueueBack :: a -> RealTimeDeque a -> RealTimeDeque a
enqueueBack x (RTDeque front back) = RTDeque front (x : back)

dequeueFront :: RealTimeDeque a -> Maybe (a, RealTimeDeque a)
dequeueFront (RTDeque [] []) = Nothing
dequeueFront (RTDeque [] back) = dequeueFront (RTDeque (reverse back) [])
dequeueFront (RTDeque (x:xs) back) = Just (x, RTDeque xs back)

dequeueBack :: RealTimeDeque a -> Maybe (a, RealTimeDeque a)
dequeueBack (RTDeque [] []) = Nothing
dequeueBack (RTDeque front []) = dequeueBack (RTDeque [] (reverse front))
dequeueBack (RTDeque front (x:xs)) = Just (x, RTDeque front xs)


main :: IO ()
main = do
    let deque = emptyDeque
    let deque1 = enqueueFront 1 deque
    let deque2 = enqueueBack 2 deque1
    let (frontElem, deque3) = case dequeueFront deque2 of
                                Just (x, dq) -> (x, dq)
                                Nothing -> error "Deque is empty"
    putStrLn $ "Front element: " ++ show frontElem
    putStrLn $ "Is the deque empty? " ++ show (isEmpty deque3)