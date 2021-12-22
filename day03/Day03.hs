module Main (main) where

import Data.Bits (bit, complement, shiftL, (.|.))
import Data.List (transpose)
import Util.Advent (showResult, tbd)
import Debug.Trace (traceShow)

main = showResult part1 part2

part1 input = gamma diags * epsilon diags
  where
    diags = diagnostics input

part2 input = oxygen diags * co2 diags
  where
    diags = diagnostics input

diagnostics = transpose . map (map parseDigit) . lines

gamma = binary . map commonDigit

epsilon = binary . map ((1-) . commonDigit)

oxygen = binary . filterDigits commonDigit

co2 = binary . filterDigits ((1-) . commonDigit)

filterDigits selector [] = []
filterDigits selector ([] : xss) = []
filterDigits selector ([x] : xss) = x : filterDigits selector xss
filterDigits selector (xs : xss) = digit : filterDigits selector (map filterMatches xss)
  where
    digit = selector xs
    filterMatches = filterWhen [x == digit | x <- xs]
    filterWhen bs xs = [x | (b, x) <- zip bs xs, b]

binary = binary' . reverse

binary' [] = 0
binary' (x : xs) = x .|. binary' xs `shiftL` 1

commonDigit xs = if 2 * sum xs >= length xs then 1 else 0

parseDigit '0' = 0
parseDigit '1' = 1
parseDigit _ = error "Bad digit"
