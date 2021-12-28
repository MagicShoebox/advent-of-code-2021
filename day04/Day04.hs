module Main (main) where

import Data.List (find, transpose)
import Debug.Trace (traceShow)
import Util.Advent (showResult, tbd)
import Util.String (split)

type Draws = [Int]

type Board = [[Int]]

main = showResult part1 part2

part1 input = score draws board
  where
    (draws, board) = firstWinner 1 allDraws boards
    (allDraws, boards) = parseInput input

part2 input = score draws board
  where
    (draws, board) = lastWinner (length allDraws) allDraws boards
    (allDraws, boards) = parseInput input

score draws board = last draws * sum (map (sum . unmatched) board)
  where
    unmatched = filter (not . (`elem` draws))

firstWinner n draws boards = case find (isWinner $ take n draws) boards of
  Just board -> (take n draws, board)
  Nothing -> firstWinner (n + 1) draws boards

lastWinner n draws boards = case find (not . isWinner (take n draws)) boards of
  Just board -> (take (n+1) draws, board)
  Nothing -> lastWinner (n - 1) draws boards

isWinner :: Draws -> Board -> Bool
isWinner draws board = any winner board || any winner (transpose board)
  where
    winner = all (`elem` draws)

parseInput :: String -> (Draws, [Board])
parseInput input = (parseDraws draws, parseBoards boards)
  where
    draws = head $ lines input
    boards = drop 2 $ lines input

parseDraws = map read . split ","

parseBoards [] = []
parseBoards input = parseBoard board : parseBoards rest
  where
    board = takeWhile (/= "") input
    rest = drop 1 $ dropWhile (/= "") input

parseBoard = map (map read . words)
