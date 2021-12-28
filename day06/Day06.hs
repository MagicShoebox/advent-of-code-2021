module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.List (frequency)
import Util.String (split)

main = showResult part1 part2

part1 input = sum [pop age 80 | age <- [0 .. 8]]
  where
    pop = population (parseInput input)

part2 = tbd

population initial age day
  | day < 0 = 0
  | day == 0 = fromMaybe 0 (lookup age initial)
  | age == 6 = population initial 7 (day - 1) + population initial 0 (day - 1)
  | age == 8 = population initial 0 (day - 1)
  | otherwise = population initial (age + 1) (day - 1)

parseInput :: String -> [(Int, Int)]
parseInput = map swap . frequency . map read . split ","
