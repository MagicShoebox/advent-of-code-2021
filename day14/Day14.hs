module Main (main) where

import Control.Arrow ((&&&))
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.List (frequency)

main = showResult part1 part2

part1 input = uncurry (-) $ (maximum &&& minimum) $ map fst $ frequency final
  where
    final = iterate (step rules) template !! 10
    (template, rules) = parseInput input

part2 = tbd

step rules template = inserted ++ [last template]
  where
    inserted = concat $ zipWith insert template $ drop 1 template
    insert a b = [a, fromJust $ lookup (a, b) rules]

parseInput input = (template, rules)
  where
    (template : _ : ruleLines) = lines input
    rules = map (\(a : b : cs) -> ((a, b), last cs)) ruleLines
