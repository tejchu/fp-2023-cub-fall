module Tree3 where

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

