-- TODO: 
-- Create BootstrappedHeap instance of heap

module BootstrappedHeap where
    import qualified SkewBinomialHeap as SBH

    data BootstrappedHeap a = Empty | NonEmpty (SBH.SkewBinomialHeap a) deriving (Show, Eq, Ord)

    createEmpty :: Ord a => BootstrappedHeap a
    createEmpty = Empty

    isEmpty :: Ord a => BootstrappedHeap a -> Bool
    isEmpty Empty = True
    isEmpty _ = False
    
    findMin :: Ord a => BootstrappedHeap a -> a
    findMin Empty = error "empty heap"
    findMin (NonEmpty h) = SBH.findMin h

    deleteMin :: Ord a => BootstrappedHeap a -> BootstrappedHeap a
    deleteMin Empty = error "empty heap"
    deleteMin (NonEmpty h) = NonEmpty (SBH.deleteMin h)

    insert :: Ord a => a -> BootstrappedHeap a -> BootstrappedHeap a
    insert x Empty = NonEmpty (SBH.insert x SBH.createEmpty)
    insert x (NonEmpty h) = NonEmpty (SBH.insert x h)

    merge :: Ord a => BootstrappedHeap a -> BootstrappedHeap a -> BootstrappedHeap a
    merge Empty h = h
    merge h Empty = h
    merge (NonEmpty h1) (NonEmpty h2) = NonEmpty (SBH.merge h1 h2)
    
    insertAll :: Ord a => [a] -> BootstrappedHeap a -> BootstrappedHeap a
    insertAll [] h = h
    insertAll (x:xs) h = insertAll xs (insert x h)

    fromList :: Ord a => [a] -> BootstrappedHeap a
    fromList [] = Empty
    fromList xs = NonEmpty (SBH.fromList xs)

