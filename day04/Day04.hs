module Main (main) where

import Data.List (find, transpose)
import Data.Text (pack, splitOn, unpack)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)

type Draws = [Int]

type Board = [[Int]]

main = showResult part1 part2

part1 input = score draws board
  where
    (draws, board) = firstWinner 1 allDraws boards
    (allDraws, boards) = parseInput input

part2 = tbd

score draws board = last draws * sum (map (sum . filter (not . (`elem` draws))) board)

firstWinner n draws boards = case find (isWinner $ take n draws) boards of
  Just board -> (take n draws, board)
  Nothing -> firstWinner (n + 1) draws boards

isWinner :: Draws -> Board -> Bool
isWinner draws board = any winner board || any winner (transpose board)
  where
    winner = all (`elem` draws)

parseInput :: String -> (Draws, [Board])
parseInput input = (parseDraws draws, parseBoards boards)
  where
    draws = head $ lines input
    boards = drop 2 $ lines input

parseDraws = map (read . unpack) . splitOn (pack ",") . pack

parseBoards [] = []
parseBoards input = parseBoard board : parseBoards rest
  where
    board = takeWhile (/= "") input
    rest = drop 1 $ dropWhile (/= "") input

parseBoard = map (map read . words)
