-- TODO: 
-- Create SkewBinomialHeap instance of heap

module SkewBinomialHeap (SkewBinomialHeap, createEmpty, isEmpty, merge, insert, insertAll, deleteMin, findMin) where

data Tree a = Node { rank :: Int, root :: a, singles :: [a], children :: [Tree a] } deriving (Show, Eq, Ord)
type SkewBinomialHeap a = [Tree a]

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 = 
    let Node r y ys c = link t1 t2 
    in if x <= y 
        then Node r x (y:ys) c 
        else Node r y (x:ys) c

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r1 x1 xs1 c1) t2@(Node r2 x2 xs2 c2) = 
    if x1 <= x2 
        then Node (r1+1) x1 xs1 (t2:c1) 
        else Node (r2+1) x2 xs2 (t1:c2)

insert :: Ord a => a -> SkewBinomialHeap a -> SkewBinomialHeap a
insert x [] = [Node 0 x [] []]
insert x [t] = Node 0 x [] [] : [t]
insert x ts@(t1:t2:rs) =
    if rank t1 == rank t2 
        then skewLink x t1 t2 : rs 
        else Node 0 x [] [] : ts

findMin :: Ord a => SkewBinomialHeap a -> a
findMin [] = error "empty heap"
findMin [t] = root t
findMin (t:ts) = let x = root t 
                     y = findMin ts
                    in if x <= y 
                        then x 
                        else y

getMin :: Ord a => SkewBinomialHeap a -> (Tree a, SkewBinomialHeap a)
getMin [t] = (t, [])
getMin (t:ts) = let (t', ts') = getMin ts
                    in if root t <= root t' 
                        then (t, ts) 
                        else (t', t:ts')

insertAll :: Ord a => [a] -> SkewBinomialHeap a -> SkewBinomialHeap a
insertAll [] ts = ts
insertAll (x:xs) ts = insertAll xs (insert x ts)

deleteMin :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a
deleteMin [] = error "empty heap"
deleteMin ts = let  ((Node _ x xs c), ts') = getMin ts
                in insertAll xs (merge (reverse c) (normalize ts'))

merge :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a -> SkewBinomialHeap a
merge ts1 ts2 = mergeTrees (normalize ts1) (normalize ts2)

mergeTrees :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a -> SkewBinomialHeap a
mergeTrees ts1 [] = ts1
mergeTrees [] ts2 = ts2
mergeTrees ts1@(t1:ts1') ts2@(t2:ts2') = 
    if rank t1 < rank t2 
        then t1 : mergeTrees ts1' ts2 
        else if rank t2 < rank t1 
            then t2 : mergeTrees ts1 ts2' 
            else insTree (link t1 t2) (mergeTrees ts1' ts2')

normalize :: Ord a => SkewBinomialHeap a -> SkewBinomialHeap a
normalize [] = []
normalize (t:ts) = insTree t ts

insTree :: Ord a => Tree a -> SkewBinomialHeap a -> SkewBinomialHeap a
insTree t [] = [t]
insTree t1 ts@(t2:rs) = 
    if rank t1 < rank t2 
        then t1 : ts 
        else insTree (link t1 t2) rs

createEmpty :: Ord a => SkewBinomialHeap a
createEmpty = []

isEmpty :: Ord a => SkewBinomialHeap a -> Bool
isEmpty [] = True
isEmpty _ = False

singleton :: Ord a => a -> SkewBinomialHeap a
singleton x = [Node 0 x [] []]