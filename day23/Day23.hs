{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Array (Array, accumArray, array, assocs, bounds, elems, listArray, (!), (//))
import qualified Data.Heap as Heap (MinPrioHeap, empty, insert, view)
import Data.List (intersect, transpose, (\\))
import qualified Data.Map as Map (Map, empty, insert, (!?))
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow, traceShowId)
import Graph (Graph (Graph), Heuristic (Heuristic), astar)
import Util.Advent (showResult, tbd)

type GameState = Array Char [(Int, Int)]

main = showResult part1 part2

part1 input = score
  where
    (score, burrows) = fromJust $ solve (parseInput input)

part2 = tbd

solve burrow = astar (Graph edges) (Heuristic heuristic) (burrow, 0) end
  where
    end = burrow // [(r, a) | (a, rs) <- assocs rooms, r <- rs]

edges burrow = [(withMove burrow m, score m) | m <- ms]
  where
    ms = concatMap (moves burrow) (amphipods burrow)

heuristic (b, w) = w + sum [distance a pt (head $ rooms ! a) | (pt, a) <- amphipods b, pt `notElem` rooms ! a]
  where
    distance a (x1, y1) (x2, y2) = multiplier a * (abs (x1 - x2) + abs (y1 - y2))

amphipods burrow = filter ((`elem` ['A' .. 'D']) . snd) $ assocs burrow

withMove burrow (amphipod, move) = burrow // [(head move, '.'), (last move, amphipod)]

moves burrow (pos, amphipod) = map (amphipod,) ms
  where
    as = destinations burrow (pos, amphipod)
    ws = walkable burrow pos
    ms = [w | w <- ws, last w `elem` as, last w `notElem` forbidden]

destinations burrow (pos, amphipod)
  | pos `elem` hallway && occupied = []
  | pos `elem` hallway && not occupied = targets
  | otherwise = hallway ++ targets
  where
    targets = rooms ! amphipod
    hallway = [(x, y) | ((x, y), c) <- assocs burrow, c == '.', y == 1]
    occupied = any ((\x -> x /= '.' && x /= amphipod) . (burrow !)) targets

walkable burrow pos = tail $ walkable' [pos]
  where
    neighbors (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
    walkable' (x : xs) = case [p : x : xs | p <- neighbors x, p `notElem` xs, burrow ! p == '.'] of
      [] -> [reverse (x : xs)]
      paths -> reverse (x : xs) : concatMap walkable' paths
    walkable' [] = undefined

score (amphipod, path) = multiplier amphipod * (length path - 1)

multiplier 'A' = 1
multiplier 'B' = 10
multiplier 'C' = 100
multiplier 'D' = 1000
multiplier _ = undefined

display burrow = unlines [[burrow ! (x, y) | x <- [x1 .. x2]] | y <- [y1 .. y2]]
  where
    ((x1, y1), (x2, y2)) = bounds burrow

rooms = listArray ('A', 'D') [[(3, 2), (3, 3)], [(5, 2), (5, 3)], [(7, 2), (7, 3)], [(9, 2), (9, 3)]]

forbidden = [(3, 1), (5, 1), (7, 1), (9, 1)]

parseInput input = initial // [((x, y), c) | (y, row) <- zip [0 ..] raw, (x, c) <- zip [0 ..] row]
  where
    raw = lines input
    width = length $ transpose raw
    height = length raw
    initial = listArray ((0, 0), (width - 1, height - 1)) (repeat ' ')
