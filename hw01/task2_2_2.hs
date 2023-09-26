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
insert x s = makeBlack (ins x s)
  where
    ins :: (Ord a) => a -> Tree a -> Tree a
    ins x E = T R E x E -- New node is always red
    ins x s@(T color a y b)
      | x < y     = balance color (ins x a) y b
      | x == y    = s
      | otherwise = balance color a y (ins x b)
    
    makeBlack :: Tree a -> Tree a
    makeBlack (T _ a y b) = T B a y b

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

-- helper to properly print the tree
treeToString :: (Show a) => Tree a -> String
treeToString E = "E"
treeToString (T color left value right) =
  "(" ++ show color ++ " " ++
  treeToString left ++ " " ++
  show value ++ " " ++
  treeToString right ++ ")"

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


