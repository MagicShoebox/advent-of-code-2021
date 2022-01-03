module Main (main) where

import Control.Monad (join, void)
import Data.List (sort, (\\))
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, get, many, many1, readP_to_S, satisfy, (<++))
import Util.Advent (showResult, tbd)

data Valid = Valid

data ParseError = Incomplete String | Corrupt Char

type ParseResult = Either ParseError Valid

main = showResult part1 part2

part1 input = sum $ map score corrupt
  where
    errors = [x | (Left x) <- parseInput input]
    corrupt = [x | (Corrupt x) <- errors]
    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score '>' = 25137
    score _ = undefined

part2 input = middle $ sort $ map score incomplete
  where
    middle xs = xs !! (length xs `div` 2)
    errors = [x | (Left x) <- parseInput input]
    incomplete = [x | (Incomplete x) <- errors]
    score = foldl (\b a -> b * 5 + a) 0 . map score'
    score' ')' = 1
    score' ']' = 2
    score' '}' = 3
    score' '>' = 4
    score' _ = undefined

tokens = [('[', ']'), ('(', ')'), ('{', '}'), ('<', '>')]

parseLine = sequence <$> many1 group

group = do
  start <- choice $ map (char . fst) tokens
  many group >>= either (error start) (continue start) . sequence
  where
    error s (Incomplete xs) = incomplete s xs
    error _ (Corrupt c) = return $ Left $ Corrupt c
    continue s _ = incomplete s "" <++ valid s <++ corrupt s
    valid s = do
      char (fromJust $ lookup s tokens)
      return $ Right Valid
    incomplete s xs = do
      eof
      return $ Left $ Incomplete $ xs ++ [fromJust $ lookup s tokens]
    corrupt s = do
      e <- satisfy (`elem` map snd tokens \\ [s])
      return $ Left $ Corrupt e

parseInput = map (traverse fst . readP_to_S parseLine) . lines
