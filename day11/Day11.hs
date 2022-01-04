{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Array (Array, Ix (inRange), accum, bounds, elems, indices, listArray, (!))
import Data.Char (digitToInt)
import Data.List (transpose, (\\))
import Data.Maybe (fromMaybe, isNothing)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = fst $ iterate step (0, energy) !! 100
  where
    energy = parseInput input

part2 = tbd

step (flashes, energy) = (flashes + length (filter isNothing $ elems energy''), resetNothing energy'')
  where
    energy' = fmap (Just . (1 +)) energy
    energy'' = until allUnder9 flash energy'
    allUnder9 = all ((< 10) . fromMaybe 0)
    resetNothing = fmap (fromMaybe 0)

flash energy = accum (flip (<*>)) energy updates
  where
    flashing = filter ((> 9) . fromMaybe 0 . (energy !)) (indices energy)
    increasing = filter (inRange $ bounds energy) $ concatMap neighbors flashing
    updates = map (,Nothing) flashing ++ map (,Just (+ 1)) increasing

neighbors (x, y) = [(x + x', y + y') | x' <- [-1 .. 1], y' <- [-1 .. 1]] \\ [(x, y)]

parseInput input = listArray ((0, 0), (width - 1, height - 1)) (concat grid)
  where
    grid = map (map digitToInt) $ lines input
    width = length grid
    height = length $ transpose grid
