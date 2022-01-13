{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Arrow (second, (&&&))
import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow, traceShowId)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = losingScore * (rolls * 6 + 3)
  where
    rolls = uncurry min $ both (subtract 1 . fromJust . findIndex (>= 1000)) (p1scores, p2scores)
    losingScore = min (p1scores !! rolls) (p2scores !! rolls)
    p1scores = scores $ positions p1movements p1start
    p2scores = scores $ positions p2movements p2start
    (p1start, p2start) = parseInput input

part2 = tbd

scores = scanl (+) 0 . map (\case 0 -> 10; x -> x) . tail

positions movements start = scanl (\a b -> (a + b) `mod` 10) start movements

(p1movements, p2movements) = both cycle ([6, 4, 2, 0, 8], [5, 3, 1, 9, 7])

both f (a, b) = (f a, f b)

parseInput input = both (digitToInt . last) $ (head &&& head . tail) $ lines input
