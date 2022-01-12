{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Arrow (second, (&&&))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.List (deleteFirstsBy, find, permutations, sort, subsequences, (\\))
import Data.Map (fromListWith, (!))
import Data.Maybe (catMaybes, fromJust)
import Data.Tuple (swap)
import Debug.Trace (traceShow, traceShowId)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = length $ nubOrd $ concatMap (\(id, rs) -> map (convert id 0) rs) scanners
  where
    scanners = parseInput input
    convert = findConversion $ conversions scanners

part2 = tbd

findConversion cnvrs s1 s2 = foldl1 (.) $ map snd $ head $ findConversion' [(s1, id)]
  where
    findConversion' [] = undefined
    findConversion' (current : visited)
      | fst current == s2 = [current : visited]
      | otherwise = concatMap (findConversion' . (: current : visited)) (unseen current visited)
    unseen (current, _) visited = deleteFirstsBy ((==) `on` fst) (cnvrs ! current) visited

conversions ss = fromListWith (++) $ concat maps
  where
    guides = [(s1,s2,) <$> findGuide s1rs s2rs | [(s1, s1rs), (s2, s2rs)] <- combinations ss]
    maps = [[(s1, [(s2, toS2)]), (s2, [(s1, toS1)])] | (s1, s2, (toS2, toS1)) <- catMaybes guides]

findGuide xss yss = guide . second orient <$> findMatch overlaps
  where
    overlaps = [((xc, yc), cMatch (xs, ys)) | (xc, xs) <- centralize xss, (yc, ys) <- centralize yss]
    centralize rs =
      let center r = (r, map (zipWith subtract r) rs)
       in map center rs
    cMatch (xs, ys) = filter rMatch [(x, y) | x <- xs, y <- ys]
    rMatch (x, y) = sort (map abs x) == sort (map abs y)
    findMatch xcycms = find ((>= 12) . length . snd) xcycms
    orient ms = (orientation &&& orientation . swap) $ head $ ms \\ [([0, 0, 0], [0, 0, 0])]
    guide ((xc, yc), (xToY, yToX)) =
      ( zipWith (+) (zipWith (-) yc $ xToY xc) . xToY,
        zipWith (+) (zipWith (-) xc $ yToX yc) . yToX
      )

orientation (source, target) = (!! index) . orientations
  where
    index = fst $ fromJust $ find (and . zipWith (==) target . snd) (zip [0 ..] $ orientations source)
    orientations xyz = [zipWith (*) p [i, j, k] | p <- permutations xyz, i <- [1, -1], j <- [1, -1], k <- [1, -1]]

-- https://wiki.haskell.org/99_questions/Solutions/26
combinations xs = filter ((2 ==) . length) (subsequences xs)

parseInput :: String -> [(Int, [[Int]])]
parseInput input = map parseScanner $ scanners $ lines input
  where
    scanners [] = []
    scanners xs = let (s, ss) = break (== "") xs in s : scanners (drop 1 ss)

parseScanner [] = undefined
parseScanner (x : xs) = (header, reports)
  where
    header = read $ (x \\ "--- scanner ") \\ "---"
    reports = map parseReport xs
    parseReport [] = []
    parseReport rs = let (r, rs') = break (== ',') rs in read r : parseReport (drop 1 rs')
