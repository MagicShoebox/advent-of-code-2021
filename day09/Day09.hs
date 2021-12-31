module Main (main) where

import Data.Char (digitToInt)
import Data.List (transpose, zip5, zipWith5)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 = sum . map risk . filter isLowest . zipNeighbors . parseInput
  where
    risk (a, _, _, _, _) = a + 1

part2 = tbd

isLowest (a, b, c, d, e) = all (> a) [b, c, d, e]

zipNeighbors xss = concat $ zipWith5 zip5 center up right down left
  where
    center = offset 0 0 xss
    up = offset 0 (-1) xss
    right = offset 1 0 xss
    down = offset 0 1 xss
    left = offset (-1) 0 xss

offset x y xss = map (drop (1 + x)) $ drop (1 + y) xss

addBorder xss = [lineBuffer] ++ xss ++ [lineBuffer]
  where
    lineBuffer = replicate lineLength 10
    lineLength = length $ head xss

parseInput input = addBorder $ transpose $ addBorder $ transpose raw
  where
    raw = map (map digitToInt) $ lines input
