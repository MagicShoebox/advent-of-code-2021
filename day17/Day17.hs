{-# LANGUAGE TupleSections #-}

module Main (main) where

import Debug.Trace (traceShow, traceShowId)
import Text.Regex.PCRE ((=~))
import Util.Advent (showResult, tbd)

data TargetArea = Area {x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int}

main = showResult part1 part2

part1 input = triangle $ abs (y1 area) - 1
  where
    area = parseInput input

part2 input = concatMap (\vy -> map (,vy) (hitsArea area vy)) [-200 .. 200]
  where
    area = parseInput input

hitsArea area vy = filter hits [vx1 .. vx2]
  where
    (t1, t2) = tBounds vy (y1 area) (y2 area)
    (vx1, vx2) = vxBounds (x1 area) (x2 area)
    hits vx = any inArea [(disp vx t, height vy t) | t <- [t1 .. t2]]
    inArea (x, y) = x >= x1 area && x <= x2 area && y >= y1 area && y <= y2 area

disp vx t =
  let t' = min (abs vx) t
      op = if vx >= 0 then (-) else (+)
   in (vx * t') `op` triangle (t' - 1)

height vy t = vy * t - triangle (t - 1)

vxBounds x1 x2 = (ceiling $ ideal $ fromIntegral x1, floor $ ideal $ fromIntegral x2)
  where
    ideal x
      | x >= 0 = sqrt (1 + 8 * x) / 2 - (1 / 2)
      | otherwise = - ideal (- x)

tBounds vy y1 y2 = (ceiling $ ideal $ fromIntegral y2, floor $ ideal $ fromIntegral y1)
  where
    vy' = fromIntegral vy
    ideal h = (vy' + 1 / 2) + sqrt (vy' * vy' + vy' - (2 * h) + 1 / 4)

triangle x = (x * (x + 1)) `div` 2

parseInput :: String -> TargetArea
parseInput input = Area {x1 = read x1, x2 = read x2, y1 = read y1, y2 = read y2}
  where
    targetPattern = "target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)"
    [[_, x1, x2, y1, y2]] = input =~ targetPattern
