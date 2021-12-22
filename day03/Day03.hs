module Main (main) where

import Data.Bits (complement, shiftL, (.|.))
import Data.List (transpose)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 :: String -> Int
part1 input = gamma diagsT * epsilon diagsT
  where
    diagsT = transpose (diagnostics input)

part2 = tbd

diagnostics input = map (map parseDigit) (lines input)

gamma = commonBinary 0

epsilon = complement . commonBinary (-1)

commonBinary n numbers = foldl binaryReducer n $ zip (map sum numbers) (map length numbers)
  where
    binaryReducer g (s, l) = (g `shiftL` 1) .|. (if s > l `div` 2 then 1 else 0)

parseDigit '0' = 0
parseDigit '1' = 1
parseDigit _ = error "Bad digit"
