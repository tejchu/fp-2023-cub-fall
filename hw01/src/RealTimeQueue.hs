module RealTimeQueue (module Queue, RealTimeQueue) where
  import Queue

  data RealTimeQueue a = RTQ [a] [a] [a]

  checkInvariant (RTQ frontList revList (s:xs)) = (RTQ frontList revList xs)
  checkInvariant (RTQ frontList revList []) = 
    let list = rotate frontList revList []
    in RTQ list [] list

  rotate [] [y] a = y:a
  rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)

  instance Queue RealTimeQueue where
    createEmpty = RTQ [] [] []
    
    isEmpty (RTQ [] [] []) = True
    isEmpty _              = False

    enqueue (RTQ frontList revList strList) x = checkInvariant (RTQ frontList (x:revList) strList)

    head (RTQ [] _ _) = error "Try head on empty queue"
    head (RTQ (x:f) revList strList) = x

    tail (RTQ [] _ _) = error "Try tail on empty queue"
    tail (RTQ (x:f) revList strList) = checkInvariant (RTQ f revList strList)