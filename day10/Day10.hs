module Main (main) where

import Control.Monad (join, void)
import Data.List ((\\))
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, get, many, many1, readP_to_S, satisfy, (<++))
import Util.Advent (showResult, tbd)

data Valid = Valid

data ParseError = Incomplete Char | Corrupt Char

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

part2 = tbd

tokens = [('[', ']'), ('(', ')'), ('{', '}'), ('<', '>')]

parseLine = sequence <$> many1 group

group = do
  start <- choice $ map (char . fst) tokens
  many group >>= either (return . Left) (const $ incomplete start <++ valid start <++ corrupt start) . sequence
  where
    valid s = do
      char (fromJust $ lookup s tokens)
      return $ Right Valid
    incomplete s = do
      eof
      return $ Left $ Incomplete s
    corrupt s = do
      e <- satisfy (`elem` map snd tokens \\ [s])
      return $ Left $ Corrupt e

parseInput = map (traverse fst . readP_to_S parseLine) . lines
