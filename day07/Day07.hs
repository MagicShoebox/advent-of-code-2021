module Main (main) where

import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.String (split)

main = showResult part1 part2

part1 input = minimum $ fuel metric xs
  where
    metric pos = abs . (pos -)
    xs = parseInput input

part2 input = minimum $ fuel metric xs
  where
    metric pos = diff . abs . (pos -)
    diff x = (x * (x + 1)) `div` 2
    xs = parseInput input

fuel metric xs = [sum $ map (metric pos) xs | pos <- range]
  where
    range = [(minimum xs) .. (maximum xs)]

parseInput :: String -> [Int]
parseInput = map read . split ","
