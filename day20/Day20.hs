module Main (main) where

import Control.Arrow ((&&&))
import Data.Array (listArray, (!))
import Data.List (transpose)
import Debug.Trace (trace, traceShow, traceShowId)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = sum $ map (length . filter (== '#')) $ snd final
  where
    final = iterate (second' expand . enhance algorithm) ('.', expand '.' image) !! 2
    (algorithm, image) = parseInput input

part2 = tbd

enhance algorithm (pad, image) = (nextPad, map (map pixel) reordered)
  where
    width = length $ transpose image
    height = length image
    expanded = expand pad image
    window = [(dx, dy) | dy <- [-1 .. 1], dx <- [-1 .. 1]]
    windows = [map (sub (1 + dx) width) $ sub (1 + dy) height expanded | (dx, dy) <- window]
    sub skip length = take length . drop skip
    reordered = map transpose $ transpose windows
    pixel = (algorithm !) . foldl (\a b -> a * 2 + digit b) 0
    digit x = if x == '#' then 1 else 0
    nextPad = algorithm ! if pad == '#' then 511 else 0

expand c image = border $ transpose $ border $ transpose image
  where
    border xss = [buffer xss] ++ xss ++ [buffer xss]
    buffer xss = replicate (length $ head xss) c

second' f (a, b) = (a, f a b)

parseInput input = (algorithm, image)
  where
    (algorithmLine, image) = (head &&& drop 2) $ lines input
    algorithm = listArray (0, 511) algorithmLine
