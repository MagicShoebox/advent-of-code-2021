module Util.Advent (tbd, putResult, showResult) where
import Util.Input (withInput)

tbd input = "TBD"

showResult part1 part2 = putResult (show . part1) (show . part2)

putResult part1 part2 = withInput $ \input -> do
  putStrLn "Part 1:"
  putStrLn $ part1 input
  putStrLn ""
  putStrLn "Part 2:"
  putStrLn $ part2 input
