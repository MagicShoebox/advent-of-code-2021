module Main (main) where

import Data.Char (digitToInt)
import Data.List (find, sortOn, transpose, union, zip5, zipWith5)
import Data.Ord (Down (Down), comparing)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = sum $ map risk lowest
  where
    lowest = filter (isLowest heights) points
    (points, heights) = parseInput input
    risk p = heights p + 1

part2 input = fmap (product . take 3 . sortOn Down . map length) basins
  where
    basins = find ((== maxBasins) . sum . map length) (iterate fill lowest)
    fill = map (grow heights)
    maxBasins = length $ filter (< 9) $ map heights points
    lowest = map (: []) $ filter (isLowest heights) points
    (points, heights) = parseInput input

grow heights basin = basin `union` filter ((< 9) . heights) border
  where
    border = filter (`notElem` basin) $ concatMap neighbors basin

isLowest hs p = all ((> hs p) . hs) (neighbors p)

neighbors (x, y) = [up, right, down, left]
  where
    up = (x, y - 1)
    right = (x + 1, y)
    down = (x, y + 1)
    left = (x - 1, y)

parseInput input = (points, \(x, y) -> heights !! x !! y)
  where
    raw = map (map digitToInt) $ lines input
    heights = addBorder $ transpose $ addBorder $ transpose raw
    addBorder xss = [lineBuffer xss] ++ xss ++ [lineBuffer xss]
    lineBuffer xss = replicate (length $ head xss) 10
    points = [(x, y) | x <- [1 .. length raw], y <- [1 .. length $ transpose raw]]
