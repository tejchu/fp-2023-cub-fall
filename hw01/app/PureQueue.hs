module PureQueue (module Queue, PureQueue) where
  import Queue

  data PureQueue a = Q [a] [a]

  instance Queue PureQueue where
    createEmpty = Q [] [] 
    isEmpty (Q [] []) = True
    isEmpty (Q _ _) = False

    enqueue (Q [] []) x = (Q [x] [])
    enqueue (Q frontList revList) x = (Q frontList (x:revList))

    head (Q [] _ ) = error "Try head on empty queue"
    head (Q (x:f) revList) = x

    tail (Q [x] revList) = (Q (reverse revList) [])
    tail (Q (x:f) revList) = (Q f revList)