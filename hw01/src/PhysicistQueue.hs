module PhysicistQueue (module Queue, PhysicistQueue) where
  import Queue

  data PhysicistQueue a = PQ [a] [a] Int [a] Int 

  checkInvariant q@(PQ w frontList lenf revList lenr)
      | lenr <= lenf = checkw q
      | otherwise = checkw (PQ frontList (frontList ++ reverse revList) (lenf+lenr) [] 0)

  checkw (PQ [] frontList lenf revList lenr) = (PQ frontList frontList lenf revList lenr)
  checkw (PQ w frontList lenf revList lenr) = (PQ w frontList lenf revList lenr)

  instance Queue PhysicistQueue where
    createEmpty = PQ [] [] 0 [] 0
    isEmpty (PQ w frontList lenf revList lenr) = (lenf == 0)

    enqueue (PQ w frontList lenf revList lenr) x = checkInvariant (PQ w frontList lenf (x:revList) (lenr+1))

    head (PQ [] frontList lenf revList lenr) = error "Try head on empty queue"
    head (PQ (x:w) frontList lenf revList lenr) = x

    tail (PQ [] frontList lenf revList lenr) = error "Try tail on empty queue"
    tail (PQ (x:w) frontList lenf revList lenr) = checkInvariant (PQ w (Prelude.tail frontList) (lenf-1) revList lenr)