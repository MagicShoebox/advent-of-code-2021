module Main (main) where

import Data.Array (accum, bounds, inRange, indices, listArray, (!), (//))
import Data.Char (digitToInt)
import Data.List (minimumBy, transpose)
import Data.Ord (comparing)
import Data.Set (delete, fromList, member)
import Debug.Trace (traceShow)
import GHC.Base (maxInt)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = uncurry (risk cave) (bounds cave)
  where
    cave = parseInput input

part2 = tbd

risk cave start end = risk' initialRisk (fromList $ indices cave) [start]
  where
    initialRisk = (maxInt <$ cave) // [(start, 0)]
    risk' risks unvisited [] = undefined
    risk' risks unvisited (pt : visited)
      | pt == end = risks ! pt
      | otherwise = risk' newRisks newUnvisited (next : pt : visited)
      where
        nhbrs = filter (`member` unvisited) $ filter (inRange $ bounds cave) (neighbors pt)
        nhbrRisks = map (\nhbr -> (nhbr, (risks ! pt) + (cave ! nhbr))) nhbrs
        newRisks = accum min risks nhbrRisks
        newUnvisited = delete pt unvisited
        next = minimumBy (comparing (risks !)) newUnvisited

neighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

parseInput input = listArray ((0, 0), (width - 1, height - 1)) raw
  where
    raw = concatMap (map digitToInt) $ lines input
    width = length $ transpose $ lines input
    height = length $ lines input
