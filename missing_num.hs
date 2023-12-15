import Data.Array

-- O(n) time, O(n) space
countingSort :: (Ix n) => [n] -> n -> n -> [n]
countingSort l lo hi = concat [replicate times n | (n, times) <- counts]
  where counts = assocs (accumArray (+) 0 (lo, hi) [(i, 1) | i <- l])

-- O(n) time, O(1) space
findMissing :: (Ix n, Enum n) => [n] -> n
findMissing xs = findMissing' (countingSort xs (minimum xs) (maximum xs)) (minimum xs)

findMissing' :: (Eq n, Enum n) => [n] -> n -> n
findMissing' [] n = n
findMissing' (x:xs) n
  | x == n    = findMissing' xs (succ n)
  | otherwise = n

main :: IO ()
main = do
  let inputList = [3, 1, 4, 5, 6]
  putStrLn $ "Input List: " ++ show inputList
  putStrLn $ "Smallest missing number: " ++ show (findMissing inputList)
