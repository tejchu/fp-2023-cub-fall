module Queue (Queue(..)) where
  import Prelude hiding (head,tail)

  class Queue q where
    createEmpty :: q a
    isEmpty :: q a -> Bool

    enqueue :: q a -> a -> q a
    head    :: q a -> a
    tail    :: q a -> q a