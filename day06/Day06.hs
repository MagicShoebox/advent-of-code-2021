module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.List (frequency)
import Util.String (split)

main = showResult part1 part2

part1 input = population (parseInput input) 80

part2 input = population (parseInput input) 256

-- Memoization: https://stackoverflow.com/a/5553390/3491874
population initial day = sum [population' day age | age <- [0 .. 8]]
  where
    population' 0 age = fromMaybe 0 (lookup age initial)
    population' day age = memo !! day !! age
    memo = map (\x -> map (pop x) [0 ..]) [0 ..]
    pop day 6 = sum (map (population' (day - 1)) [0, 7])
    pop day age = population' (day - 1) ((age + 1) `mod` 9)

parseInput :: String -> [(Int, Int)]
parseInput = frequency . map read . split ","
