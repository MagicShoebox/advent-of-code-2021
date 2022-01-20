{-# LANGUAGE TupleSections #-}

module Graph (Graph (Graph), Heuristic (Heuristic), astar, dijkstra) where

import qualified Data.Heap as Heap (MinPrioHeap, empty, insert, view)
import qualified Data.Map as Map (Map, empty, insert, singleton, (!?))
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

newtype Graph n w = Graph (n -> [(n, w)])

newtype Heuristic n w = Heuristic ((n, w) -> w)

type State n w = Maybe ((w, n), Heap.MinPrioHeap w n)

dijkstra graph (start, startW) end = go graph heuristic end dists Map.empty initial
  where
    heuristic = Heuristic snd
    dists = Map.singleton start startW
    initial = Just ((0, start), Heap.empty)

astar graph heuristic (start, startW) end = go graph heuristic end dists Map.empty initial
  where
    dists = Map.singleton start startW
    initial = Just ((0, start), Heap.empty)

go :: (Ord n, Bounded w, Num w, Ord w) => Graph n w -> Heuristic n w -> n -> Map.Map n w -> Map.Map n n -> State n w -> Maybe (w, [n])
go _ _ _ _ _ Nothing = Nothing
go (Graph edges) (Heuristic heuristic) end dists prevs (Just ((_, node), queue))
  | node == end = (,reverse $ follow prevs end) <$> dists Map.!? end
  | otherwise = go (Graph edges) (Heuristic heuristic) end newDists newPrev $ Heap.view newQueue
  where
    dist x = fromMaybe maxBound $ dists Map.!? x
    paths = [(n, dist node + w) | (n, w) <- edges node]
    shorter = [(e, d) | (e, d) <- paths, d < dist e]
    newDists = foldr (uncurry Map.insert) dists shorter
    newPrev = foldr (uncurry Map.insert) prevs [(e, node) | (e, _) <- shorter]
    newQueue = foldr Heap.insert queue [(heuristic (e, d), e) | (e, d) <- shorter]

follow prevs x = case prevs Map.!? x of
  Nothing -> []
  (Just y) -> x : follow prevs y
