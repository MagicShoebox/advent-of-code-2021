{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Array (Array, assocs, bounds, listArray, (!), (//))
import qualified Data.Heap as Heap (MinPrioHeap, empty, insert, view)
import Data.List (intersect, transpose, (\\))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set (Set, empty, fromList, insert, member, notMember, size, toList)
import Debug.Trace (traceShow, traceShowId)
import Graph (Graph (Graph), Heuristic (Heuristic), astar)
import Util.Advent (showResult, tbd)

data Board = Board
  { rooms :: Array Char (Set.Set (Int, Int)),
    hallway :: Set.Set (Int, Int),
    forbidden :: Set.Set (Int, Int)
  }

data Path = Path
  { start :: (Int, Int),
    trail :: Set.Set (Int, Int),
    end :: (Int, Int)
  }

main = showResult part1 part2

part1 input = score
  where
    burrow = parseInput input
    game = board burrow
    (score, burrows) = fromJust $ solve game burrow

part2 input = score
  where
    (top, bottom) = splitAt 3 $ lines input
    expanded = unlines $ top ++ ["  #D#C#B#A#", "  #D#B#A#C#"] ++ bottom
    burrow = parseInput expanded
    game = board burrow
    (score, burrows) = fromJust $ solve game burrow

solve game burrow = astar (Graph $ edges game) (Heuristic $ heuristic game) (burrow, 0) end
  where
    end = burrow // [(r, a) | (a, rs) <- assocs $ rooms game, r <- Set.toList rs]

edges game burrow = [(withMove burrow m, score m) | m <- ms]
  where
    ms = concatMap (moves game burrow) (amphipods burrow)

heuristic Board {rooms} (b, w) = w - sum [multiplier a | (pt, a) <- amphipods b, inRoom (pt, a)]
  where
    inRoom (pt, a) = pt `Set.member` (rooms ! a)

amphipods burrow = [(pt, a) | (pt, a) <- assocs burrow, a == 'A' || a == 'B' || a == 'C' || a == 'D']

withMove burrow (amphipod, move) = burrow // [(start move, '.'), (end move, amphipod)]

moves game burrow (pos, amphipod) = map (amphipod,) ms
  where
    as = destinations game burrow (pos, amphipod)
    ws = walkable burrow pos
    ms = [w | w <- ws, end w `Set.member` as, end w `Set.notMember` forbidden game]

destinations Board {hallway, rooms} burrow (pos, amphipod)
  | pos `Set.member` hallway && occupied = Set.empty
  | pos `Set.member` hallway && not occupied = targets
  | pos `Set.member` targets && occupied = hallway
  | pos `Set.member` targets && not occupied = Set.empty
  | occupied = hallway
  | otherwise = targets
  where
    targets = rooms ! amphipod
    occupied = any ((\x -> x /= '.' && x /= amphipod) . (burrow !)) targets

walkable burrow pos = tail $ walkable' $ Path pos Set.empty pos
  where
    neighbors (x, y) = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
    walkable' Path {start, trail, end} =
      let trail' = Set.insert end trail
       in case [Path start trail' p | p <- neighbors end, p `Set.notMember` trail, burrow ! p == '.'] of
            [] -> [Path start trail' end]
            paths -> Path start trail' end : concatMap walkable' paths

score (amphipod, Path {trail}) = multiplier amphipod * (Set.size trail - 1)

multiplier 'A' = 1
multiplier 'B' = 10
multiplier 'C' = 100
multiplier 'D' = 1000
multiplier _ = undefined

display burrow = unlines [[burrow ! (x, y) | x <- [x1 .. x2]] | y <- [y1 .. y2]]
  where
    ((x1, y1), (x2, y2)) = bounds burrow

board burrow = Board rooms hallway forbidden
  where
    rooms = listArray ('A', 'D') $ map (Set.fromList . take heights) roomPoints
    hallway = Set.fromList [(x, y) | ((x, y), c) <- assocs burrow, c == '.', y == 1]
    forbidden = Set.fromList [(x, y -1) | ((x, y) : _) <- roomPoints]
    heights = snd (snd (bounds burrow)) - 2
    roomPoints = map (\x -> zip (repeat x) [2 .. 5]) [3, 5, 7, 9]

parseInput input = initial // [((x, y), c) | (y, row) <- zip [0 ..] raw, (x, c) <- zip [0 ..] row]
  where
    raw = lines input
    width = length $ transpose raw
    height = length raw
    initial = listArray ((0, 0), (width - 1, height - 1)) (repeat ' ')
