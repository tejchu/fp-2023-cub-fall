import Criterion.Main

data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

member :: (Ord a) => a -> Tree a -> Bool
member x E    = False
member x (T _ a y b)
  | x < y     = member x a
  | x == y    = True
  | otherwise = member x b

insert :: (Ord a) => a -> Tree a -> Tree a
insert x s = makeBlack $ ins s
  where ins E  = T R E x E
        ins (T color a y b)
          | x < y  = balance color (ins a) y b
          | x == y = T color a y b
          | x > y  = balance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

-- delete function if needed
balance' :: Tree a -> Tree a
balance' (T color left value right) = balance color left value right

delete :: (Ord a) => a -> Tree a -> Tree a
delete x t = makeBlack $ del x t
  where makeBlack (T _ a y b) = T B a y b
        makeBlack E           = E

del :: (Ord a) => a -> Tree a -> Tree a
del x t@(T _ l y r)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = fuse l r

delL :: (Ord a) => a -> Tree a -> Tree a
delL x t@(T B t1 y t2) = balL $ T B (del x t1) y t2
delL x t@(T R t1 y t2) = T R (del x t1) y t2

balL :: Tree a -> Tree a
balL (T B (T R t1 x t2) y t3) = T R (T B t1 x t2) y t3
balL (T B t1 y (T B t2 z t3)) = balance' (T B t1 y (T R t2 z t3))
balL (T B t1 y (T R (T B t2 u t3) z t4@(T B l value r))) =
  T R (T B t1 y t2) u (balance' (T B t3 z (T R l value r)))

delR :: (Ord a) => a -> Tree a -> Tree a
delR x t@(T B t1 y t2) = balR $ T B t1 y (del x t2)
delR x t@(T R t1 y t2) = T R t1 y (del x t2)

balR :: Tree a -> Tree a
balR (T B t1 y (T R t2 x t3)) = T R t1 y (T B t2 x t3)
balR (T B (T B t1 z t2) y t3) = balance' (T B (T R t1 z t2) y t3)
balR (T B (T R t1@(T B l value r) z (T B t2 u t3)) y t4) =
  T R (balance' (T B (T R l value r) z t2)) u (T B t3 y t4)

fuse :: Tree a -> Tree a -> Tree a
fuse E t = t
fuse t E = t
fuse t1@(T B _ _ _) (T R t3 y t4) = T R (fuse t1 t3) y t4
fuse (T R t1 x t2) t3@(T B _ _ _) = T R t1 x (fuse t2 t3)
fuse (T R t1 x t2) (T R t3 y t4)  =
  let s = fuse t2 t3
  in case s of
       (T R s1 z s2) -> (T R (T R t1 x s1) z (T R s2 y t4))
       (T B _ _ _)   -> (T R t1 x (T R s y t4))
fuse (T B t1 x t2) (T B t3 y t4)  =
  let s = fuse t2 t3
  in case s of
       (T R s1 z s2) -> (T R (T B t1 x s1) z (T B s2 y t4))
       (T B s1 z s2) -> balL (T B t1 x (T B s y t4))


-- helper to properly print the tree
treeToString :: (Show a) => Tree a -> String
treeToString E = "E"
treeToString (T color left value right) =
  "(" ++ show color ++ " " ++
  treeToString left ++ " " ++
  show value ++ " " ++
  treeToString right ++ ")"

-- New addition
fromOrdList :: (Ord a) => [a] -> Tree a
fromOrdList xs = foldl (\tree x -> insert x tree) E xs


sampleTree1 :: Tree Int
sampleTree1 = T B E 42 E

sampleTree2 :: Tree Int
sampleTree2 =
  T B
    (T R (E) 5 (E))
    10
    (T R (E) 15 (E))

sampleTree3 :: Tree Int
sampleTree3 =
  T B
    (T R (E) 5 (E))
    10
    (T R (T B (E) 20 (E)) 30 (E))

main :: IO ()
main = defaultMain
  [ bench "insert 1000 into sampleTree2" $ whnf (\n -> insert n sampleTree2) 1000
  , bench "insert 10000 into sampleTree2" $ whnf (\n -> insert n sampleTree2) 10000
  , bench "insert 42 into sampleTree1" $ whnf (\n -> insert n sampleTree1) 42
  , bench "insert 5 into sampleTree1" $ whnf (\n -> insert n sampleTree1) 5
  , bench "insert 20 into sampleTree3" $ whnf (\n -> insert n sampleTree3) 20
  , bench "insert 25 into sampleTree3" $ whnf (\n -> insert n sampleTree3) 25
  ]


