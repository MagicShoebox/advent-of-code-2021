module Main (main) where

import Control.Arrow ((&&&))
import Data.Array (accum, array, assocs, elems, listArray, (!), (//))
import Debug.Trace (traceShow, traceShowId)
import Util.Advent (showResult, tbd)
import Util.List (frequency)

main = showResult part1 part2

part1 input = range $ filter (> 0) $ elems freqs
  where
    freqs = roots (branch rules) 10 template
    (template, rules) = parseInput input

part2 input = range $ filter (> 0) $ elems freqs
  where
    freqs = roots (branch rules) 40 template
    (template, rules) = parseInput input

initial = listArray ('A', 'Z') (repeat 0)

range = uncurry (-) . (maximum &&& minimum)

merge arr1 arr2 = accum (+) arr1 $ assocs arr2

roots f n (x : y : xs) = f (n, x, y) `merge` roots f n (y : xs)
roots f n (x : xs) = initial // [(x, 1)]
roots f n [] = initial

branch rules = (!) memo
  where
    memo = array ((0, 'A', 'A'), (40, 'Z', 'Z')) [((n, x, y), branch' x y n) | n <- [0 .. 40], x <- ['A' .. 'Z'], y <- ['A' .. 'Z']]
    branch' x z 0 = initial // [(x, 1)]
    branch' x z n = left `merge` right
      where
        y = rules ! (x, z)
        left = memo ! (n - 1, x, y)
        right = memo ! (n - 1, y, z)

parseInput input = (template, array (('A', 'A'), ('Z', 'Z')) rules)
  where
    (template : _ : ruleLines) = lines input
    rules = map (\(a : b : cs) -> ((a, b), last cs)) ruleLines
