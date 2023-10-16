module BankersQueue (module Queue, BankersQueue) where
  import Queue

  data BankersQueue a = BQ [a] Int [a] Int

  checkInvariant q@(BQ frontList lenf revList lenr)
      | lenr <= lenf = q
      | otherwise =  BQ (frontList ++ reverse revList) (lenf+lenr) [] 0 

  instance Queue BankersQueue where
    createEmpty = BQ [] 0 [] 0 
    isEmpty (BQ frontList lenf revList lenr) = (lenf == 0)

    enqueue (BQ frontList lenf revList lenr) x = checkInvariant (BQ frontList lenf (x:revList) (lenr+1))

    head (BQ [] _ _ _) = error "Try head on empty queue"
    head (BQ (x:f) lenf revList lenr) = x

    tail (BQ [] _ _ _) = error "Try tail on empty queue"
    tail (BQ (x:f) lenf revList lenr) = checkInvariant (BQ f (lenf-1) revList lenr)