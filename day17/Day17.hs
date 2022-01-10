module Main (main) where

import Control.Applicative (liftA2)
import Data.List (find)
import Debug.Trace (traceShow, traceShowId)
import Text.Regex.PCRE ((=~))
import Util.Advent (showResult, tbd)

data TargetArea = Area {x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int}

main = showResult part1 part2

part1 input = triangle $ abs (y1 area) - 1
  where
    area = parseInput input

part2 input = length $ filter (hitsArea area) $ liftA2 (,) [-500 .. 500] [-500 .. 500]
  where
    area = parseInput input

hitsArea area (vx, vy) = maybe False inArea end
  where
    end = find (\(x, y) -> inArea (x, y) || (y < 0 && y < y1 area)) path
    path = [(disp vx t, height vy t) | t <- [0 ..]]
    inArea (x, y) = x >= x1 area && x <= x2 area && y >= y1 area && y <= y2 area

disp vx t =
  let t' = min (abs vx) t
      op = if vx >= 0 then (-) else (+)
   in (vx * t') `op` triangle (t' - 1)

height vy t = vy * t - triangle (t - 1)

triangle x = (x * (x + 1)) `div` 2

parseInput :: String -> TargetArea
parseInput input = Area {x1 = read x1, x2 = read x2, y1 = read y1, y2 = read y2}
  where
    targetPattern = "target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)"
    [[_, x1, x2, y1, y2]] = input =~ targetPattern
