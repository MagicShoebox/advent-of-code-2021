module Main (main) where

import Util.Advent (showResult)

main = showResult (part1 . measurements) (part2 . measurements)

measurements :: String -> [Int]
measurements input = map read $ lines input

part1 = countIncreases

part2 = countIncreases . windows

countIncreases xs = length . filter id $ zipWith (<) xs (drop 1 xs)

windows xs = zipWith3 (\a b c -> a+b+c) xs (drop 1 xs) (drop 2 xs)
