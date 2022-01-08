{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Array (Array, accum, bounds, elems, inRange, indices, listArray, (!), (//))
import Data.Char (digitToInt)
import Data.Heap (MinPrioHeap, empty)
import qualified Data.Heap as Heap (fromList, insert, union, view)
import Data.List (minimumBy, transpose)
import Data.Ord (comparing)
import qualified Data.Set as Set (delete, member)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import GHC.Base (maxInt)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = uncurry (risk cave) (bounds cave)
  where
    cave = parseInput input

part2 input = uncurry (risk cave) (bounds cave)
  where
    cave = expand $ parseInput input

risk :: Array (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
risk cave start end = risk' initialRisk $ Just ((0, start), initialQueue)
  where
    initialRisk = (maxInt <$ cave) // [(start, 0)]
    initialQueue = empty :: MinPrioHeap Int (Int, Int)
    risk' risks Nothing = risks ! end
    risk' risks (Just ((_, pt), queue))
      | pt == end = risks ! pt
      | otherwise = risk' newRisks $ Heap.view newQueue
      where
        nhbrs = filter (inRange $ bounds cave) (neighbors pt)
        nhbrRisks = filter (\(n, r) -> r < risks ! n) $ map (\nhbr -> (nhbr, (risks ! pt) + (cave ! nhbr))) nhbrs
        newRisks = risks // nhbrRisks
        nhbrQueue = Heap.fromList (map swap nhbrRisks) :: MinPrioHeap Int (Int, Int)
        newQueue = queue `Heap.union` nhbrQueue

neighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

expand cave = listArray ((0, 0), (width', height')) risks
  where
    (width, height) = both (+ 1) $ snd $ bounds cave
    (width', height') = both (pred . (* 5)) (width, height)
    risks = [shift (x, y) $ cave ! (x `mod` width, y `mod` height) | x <- [0 .. width'], y <- [0 .. height']]
    shift (x, y) r = 1 + (r + (x `div` width) + (y `div` height) - 1) `mod` 9

both f (a, b) = (f a, f b)

parseInput input = listArray ((0, 0), (width - 1, height - 1)) raw
  where
    raw = concatMap (map digitToInt) $ lines input
    width = length $ transpose $ lines input
    height = length $ lines input
