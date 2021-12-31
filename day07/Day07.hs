module Main (main) where

import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.String (split)

main = showResult part1 part2

part1 input = minimum $ fuel xs
  where
    xs = parseInput input

part2 = tbd

fuel xs = [sum $ map (abs . (pos -)) xs | pos <- [(minimum xs) .. (maximum xs)]]

parseInput :: String -> [Int]
parseInput = map read . split ","
