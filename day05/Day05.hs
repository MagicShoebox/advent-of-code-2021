module Main (main) where

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.List (frequency)
import Util.String (split)

data Point = Point Int Int

data Line = Line Point Point

main = showResult part1 part2

part1 input = length (filter ((> 1) . fst) freqs)
  where
    freqs = frequency $ concatMap points lines
    lines = horzAndVert $ parseInput input

part2 input = length (filter ((> 1) . fst) freqs)
  where
    freqs = frequency $ concatMap points lines
    lines = parseInput input

points (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = zip (repeat x1) [min y1 y2 .. max y1 y2]
  | y1 == y2 = zip [min x1 x2 .. max x1 x2] (repeat y1)
  | otherwise = zip [x1, (next x1 x2) .. x2] [y1, (next y1 y2) .. y2]
  where
    next a b = if b > a then succ a else pred a

horzAndVert = filter filterLine
  where
    filterLine (Line p1 p2) = horzOrVert p1 p2
    horzOrVert (Point x1 y1) (Point x2 y2) = x1 == x2 || y1 == y2

parseInput = map (parseLine . split "->") . lines
  where
    parseLine [p1, p2] = Line (parsePoint $ split "," p1) (parsePoint $ split "," p2)
    parseLine _ = error "Bad line"
    parsePoint [x, y] = Point (read x) (read y)
    parsePoint _ = error "Bad point"
