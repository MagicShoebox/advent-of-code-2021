module Main (main) where

import Control.Arrow ((&&&))
import Data.Char (isLower)
import Data.Graph (Graph, Vertex, dfs, graphFromEdges, topSort, vertices)
import Data.List ((\\))
import Data.Map (fromListWith, toList)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.String (split)

main = showResult part1 part2

part1 input = length $ paths edges ["start"]
  where
    edges = thd . nodeFromVertex . fromJust . vertexFromKey
    (graph, nodeFromVertex, vertexFromKey) = parseInput input

part2 = tbd

paths edges ("end" : xs) = [reverse $ "end" : xs]
paths edges (x : xs) = concatMap (paths edges . (: x : xs)) unseen
  where
    unseen = edges x \\ filter small xs
    small = isLower . head
paths edges [] = []

thd (_,_,x) = x

parseInput = graphFromEdges . map (\(k, v) -> (k, k, v)) . nodes . lines
  where
    nodes = toList . fromListWith (++) . concatMap (edges . ends)
    edges (a, b) = [(a, [b]), (b, [a])]
    ends = ((!! 0) &&& (!! 1)) . split "-"
