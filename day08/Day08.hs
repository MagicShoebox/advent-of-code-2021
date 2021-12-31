module Main (main) where

import Control.Arrow (second)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

-- Digits 1, 4, 7, 8 use 2, 4, 3, 7 segments respectively
part1 input = length $ filter (`elem` [2, 3, 4, 7]) $ map length digits
    where digits = concatMap snd (parseInput input)

part2 = tbd

parseInput = map (second (drop 1) . splitAt 10 . words) . lines
