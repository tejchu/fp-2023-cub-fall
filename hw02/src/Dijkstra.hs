-- TODO:
-- 1. Implement Dijkstra's algorithm using the bootstrapped heap.
-- 2. Refactor and fix algorithm.

module Dijkstra where


import SkewBinomialHeap as Heap
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

type Graph a = Map a [(a, Int)]

dijkstra :: (Ord a, Num b) => Graph a -> a -> Map a b
dijkstra graph source = dijkstra' Heap.createEmpty Map.empty
  where
    dijkstra' pq dist
      | Heap.isEmpty pq = dist
      | otherwise = dijkstra' pq' dist'
      where
        ((distMin, minNode), pq') = Heap.findMin pq
        neighbors = Map.findWithDefault [] minNode graph
        (dist', pq'') = foldl' relax (dist, pq') neighbors

        relax (d, q) (v, weight) =
          let alt = distMin + fromIntegral weight
          in case Map.lookup v q of
            Just oldAlt | alt < oldAlt ->
              (Map.insert v alt d, insertWithPriority alt v q)
            _ ->
              (d, q)

        insertWithPriority alt v heap 
          | Nothing = Heap.insert (alt, v) heap
          | otherwise= newHeap

        -- pq'' = foldr (\(v, w) q -> Heap.insert (w, v) q) pq' neighbors
