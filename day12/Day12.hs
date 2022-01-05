module Main (main) where

import Control.Arrow ((&&&))
import Data.Char (isLower)
import Data.Containers.ListUtils (nubOrd)
import Data.List ((\\))
import Data.Map (fromListWith, (!))
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.String (split)

main = showResult part1 part2

part1 input = length $ paths unseen ["start"]
  where
    unseen visited = edges (head visited) \\ small visited
    edges = parseInput input

part2 input = length $ paths unseen ["start"]
  where
    unseen visited = edges (head visited) \\ seen (small visited)
    seen visited = if duplicates visited then visited else ["start"]
    duplicates visited = length visited /= length (nubOrd visited)
    edges = parseInput input

small = filter (isLower . head)

paths unseen ("end" : visited) = [reverse $ "end" : visited]
paths unseen visited = concatMap (paths unseen . (: visited)) (unseen visited)

parseInput = (!) . nodes . lines
  where
    nodes = fromListWith (++) . concatMap (edges . ends)
    edges (a, b) = [(a, [b]), (b, [a])]
    ends = ((!! 0) &&& (!! 1)) . split "-"
