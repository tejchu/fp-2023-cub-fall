module Tree2 where

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
          | x < y  = rbalance color (ins a) y b
          | x == y = T color a y b
          | x > y  = lbalance color a y (ins b)
        makeBlack (T _ a y b) = T B a y b

rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
rbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance color a x b = T color a x b

lbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
lbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
lbalance B (T R a x b) y c = T R (T B a x b) y (T B c z d)
  where T B c z d = c
lbalance color a x b = T color a x b

-- helper to properly print the tree
treeToString :: (Show a) => Tree a -> String
treeToString E = "E"
treeToString (T color left value right) =
  "(" ++ show color ++ " " ++
  treeToString left ++ " " ++
  show value ++ " " ++
  treeToString right ++ ")"

