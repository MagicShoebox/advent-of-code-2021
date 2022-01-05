module Main (main) where

import Control.Arrow (Arrow (second))
import Data.Char (chr)
import Data.Containers.ListUtils (nubOrd)
import Debug.Trace (traceShow)
import Util.Advent (putResult, tbd)

data Fold = Horizontal Int | Vertical Int deriving (Show)

main = putResult part1 part2

part1 input = show $ length $ fold dots (head folds)
  where
    (dots, folds) = parseInput input

part2 input = unlines [[output (x, y) | x <- [0 .. width]] | y <- [0 .. height]]
  where
    output (x, y) = if (x, y) `elem` final then chr 9608 else ' '
    (width, height) = both maximum $ unzip final
    final = foldl fold dots folds
    (dots, folds) = parseInput input

fold dots f = nubOrd $ map (pos f) dots
  where
    pos (Horizontal fx) (px, py)
      | px < fx = (px, py)
      | otherwise = (2 * fx - px, py)
    pos (Vertical fy) (px, py)
      | py < fy = (px, py)
      | otherwise = (px, 2 * fy - py)

both f (x, y) = (f x, f y)

parseInput input = (dots, folds)
  where
    breakOn x = second (drop 1) . break (== x)
    (dotLines, foldLines) = breakOn "" $ lines input
    dots = map (both read . breakOn ',') dotLines
    folds = map (parseFold . drop (length "fold along ")) foldLines
    parseFold ('x' : '=' : pos) = Horizontal $ read pos
    parseFold ('y' : '=' : pos) = Vertical $ read pos
    parseFold _ = undefined
