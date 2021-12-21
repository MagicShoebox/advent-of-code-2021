module Main (main) where

import Util.Input (putResult)
import Control.Applicative (ZipList(getZipList))

main = putResult day01

day01 input = unlines ["Part 1:", show (part1 measurements), "", "Part 2:", show (part2 measurements)]
    where measurements = map read $ lines input

part1 = countIncreases

part2 = countIncreases . windows

countIncreases xs = length . filter id $ zipWith (<) xs (drop 1 xs)
windows xs = zipWith3 (\a b c -> a+b+c) xs (drop 1 xs) (drop 2 xs)
