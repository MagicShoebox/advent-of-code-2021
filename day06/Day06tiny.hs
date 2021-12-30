module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.List (frequency)
import Util.String (split)

main = showResult part1 part2

part1 = population 80 . parseInput

part2 = population 256 . parseInput

population day = sum . (!! day) . iterate grow
  where grow = \[a,b,c,d,e,f,g,h,i] -> [b,c,d,e,f,g,a+h,i,a]

parseInput :: String -> [Int]
parseInput input = map initial [0 .. 8]
  where
    initial = fromMaybe 0 . flip lookup initial'
    initial' = map swap $ frequency $ map read $ split "," input
