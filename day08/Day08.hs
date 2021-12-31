module Main (main) where

import Data.Bifunctor (bimap)
import Data.List (elemIndex, intersect, sort, sortOn, union, (\\))
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.List (groupOn)

main = showResult part1 part2

part1 input = length $ filter ((`elem` [2, 3, 4, 7]) . length) digits
  where
    digits = concatMap snd (parseInput input)

part2 input = sum <$> traverse (fmap fromDigits . decode) (parseInput input)
  where
    decode (patterns, digits) = mapM (`elemIndex` getMap patterns) digits
    fromDigits = foldl (\a b -> a * 10 + b) 0

getMap [[one], [seven], [four], twothreefive, zerosixnine, [eight]] = map sort [abcefg, cf, acdeg, acdfg, bcdf, abdfg, abdefg, acf, abcdefg, abcdfg]
  where
    cf = one
    acf = seven
    bcdf = four
    abcdefg = eight
    abfg = foldl1 intersect zerosixnine
    adg = foldl1 intersect twothreefive
    cde = abcdefg \\ abfg
    abcefg = abcdefg \\ (adg `intersect` cde)
    acdeg = adg `union` cde
    acdfg = adg `union` cf
    abdfg = adg `union` abfg
    abdefg = abcdefg \\ (cf `intersect` cde)
    abcdfg = abfg `union` bcdf
getMap _ = error "Bad input"

parseInput = map (bimap (groupOn length . sortOn length) (map sort . drop 1) . splitAt 10 .  words) . lines
