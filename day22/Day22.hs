module Main (main) where

import Data.List (partition, transpose, zipWith4)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow, traceShowId)
import Text.Regex.PCRE ((=~))
import Util.Advent (showResult, tbd)

data RebootStep = Step {mode :: Bool, bounds :: Bounds} deriving (Eq, Ord, Show)

type Bounds = (Point, Point)

type Point = [Int]

main = showResult part1 part2

part1 input = sum $ map size $ foldl doStep [] steps
  where
    size (a, b) = product $ map ((+ 1) . abs) $ zipWith (-) a b
    steps = filter initialization $ parseInput input
    initialization (Step _ (start, end)) = all inFifty start && all inFifty end
    inFifty x = x >= -50 && x <= 50

part2 = tbd

doStep ons (Step True bounds) = ons ++ foldl (\bs x -> concatMap (without x) bs) [bounds] ons
doStep ons (Step False bounds) = concatMap (without bounds) ons

without (b1, b2) (a1, a2) = bounds
  where
    bounds = map (unzip . map fst) $ filter (any snd) $ sequence $ zipWith4 regions b1 b2 a1 a2
    regions b1 b2 a1 a2
      | b2 < a1 || b1 > a2 = [((a1, a2), True)] -- bbaa or aabb
      | b1 <= a1 && b2 < a2 = [((a1, b2), False), ((b2 + 1, a2), True)] -- baba
      | b1 <= a1 && b2 >= a2 = [((a1, a2), False)] -- baab
      | b1 > a1 && b2 < a2 = [((a1, b1 - 1), True), ((b1, b2), False), ((b2 + 1, a2), True)] -- abba
      | b1 > a1 && b2 >= a2 = [((a1, b1 - 1), True), ((b1, a2), False)] -- abab
      | otherwise = undefined -- full conditions left for clarity

parseInput input = map parseStep $ lines input

parseStep :: String -> RebootStep
parseStep line = Step (mode == "on") ([read x1, read y1, read z1], [read x2, read y2, read z2])
  where
    [[_, mode, x1, x2, y1, y2, z1, z2]] = line =~ stepPattern
    stepPattern = "(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)"
