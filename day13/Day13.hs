module Main (main) where

import Control.Arrow (Arrow (second))
import Data.Containers.ListUtils (nubOrd)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)

data Fold = Horizontal Int | Vertical Int deriving (Show)

main = showResult part1 part2

part1 input = length $ fold dots (head folds)
  where
    (dots, folds) = parseInput input

part2 = tbd

fold dots f = nubOrd $ map (pos f) dots
  where
    pos (Horizontal fx) (px, py)
      | px < fx = (px, py)
      | otherwise = (2 * fx - px, py)
    pos (Vertical fy) (px, py)
      | py < fy = (px, py)
      | otherwise = (px, 2 * fy - py)

parseInput input = (dots, folds)
  where
    breakOn x = second (drop 1) . break (== x)
    (dotLines, foldLines) = breakOn "" $ lines input
    dots = map (both read . breakOn ',') dotLines
    both f (x, y) = (f x, f y)
    folds = map (parseFold . drop (length "fold along ")) foldLines
    parseFold ('x' : '=' : pos) = Horizontal $ read pos
    parseFold ('y' : '=' : pos) = Vertical $ read pos
    parseFold _ = undefined
