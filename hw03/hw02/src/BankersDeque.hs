module BankersDeque (module Deque, BankersDeque) where
  import Deque

  c = 3

  data BankersDeque a = BD [a] Int [a] Int

  checkInvariant q@(BD frontList lenf revList lenr) =
      if lenf > c*lenr + 1 then
        let i = (lenf+lenr) `div` 2
            j = lenf+lenr-i
            f = take i frontList
            r = revList ++ reverse (drop i frontList)
        in BD f i r j
      else if lenr > c*lenf + 1 then
        let j = (lenf+lenr) `div` 2
            i = lenf+lenr-j
            r = take j revList
            f = frontList ++ reverse (drop j revList)
        in BD f i r j
      else q

  instance Deque BankersDeque where
    createEmpty = BD [] 0 [] 0
    isEmpty (BD frontList lenf revList lenr) = (lenf+lenr == 0)

    cons x (BD frontList lenf revList lenr) = checkInvariant (BD (x:frontList) (lenf+1) revList lenr)

    head (BD [] lenf revList lenr) = error "Try head on empty deque"
    head (BD (x:f) lenf revList lenr) = x

    tail (BD [] lenf revList lenr) = error "Try tail on empty deque"
    tail (BD (x:f) lenf revList lenr) = checkInvariant (BD f (lenf-1) revList lenr)

    enqueue (BD frontList lenf revList lenr) x = checkInvariant (BD frontList lenf (x:revList) (lenr+1))

    last (BD frontList lenf [] lenr) = error "Try last on empty deque"
    last (BD frontList lenf (x:r) lenr) = x

    init (BD frontList lenf [] lenr) = error "Try init on empty deque"
    init (BD frontList lenf (x:r) lenr) = checkInvariant (BD frontList lenf r (lenr-1))


  