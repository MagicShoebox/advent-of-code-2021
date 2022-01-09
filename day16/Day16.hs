module Main (main) where

import Control.Monad (liftM2, replicateM)
import Control.Monad.Trans.State (evalState, gets, modify)
import Data.Char (ord)
import Debug.Trace (traceShow, traceShowId)
import Util.Advent (showResult, tbd)

data PacketType = Operator | Literal

data LengthType = Total Int | Count Int deriving (Show)

main = showResult part1 part2

part1 = evalState packet . parseInput

part2 = tbd

packet = do
  version <- readInt 3
  pt <- packetType <$$> readInt 3
  case pt of
    Nothing -> return Nothing
    (Just Literal) -> literal >> return version
    (Just Operator) -> liftM2 (+) version <$> operator
  where
    packetType 4 = Literal
    packetType _ = Operator

literal = do
  more <- readBool
  val <- readInt 4
  case more of
    Nothing -> return Nothing
    (Just True) -> liftM2 ((+) . (16 *)) val <$> literal
    (Just False) -> return val

operator = do
  ct <- readBool
  lt <- case ct of
    Nothing -> return Nothing
    (Just False) -> Total <$$> readInt 15
    (Just True) -> Count <$$> readInt 11
  case lt of
    Nothing -> return Nothing
    (Just (Count x)) -> sum <$$> (sequence <$> replicateM x packet)
    (Just (Total x)) -> sum <$$> (evalState packets <$$> readBin x)

packets = do
  pkt <- packet
  case pkt of
    Nothing -> return []
    (Just x) -> (x :) <$> packets

readInt n = do
  foldl ((+) . (2 *)) 0 <$$> readBin n

readBool = do
  (== [1]) <$$> readBin 1

readBin n = do
  xs <- gets $ take n
  modify $ drop n
  if length xs == n then return $ Just xs else return Nothing

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

parseInput = concatMap hexToBin . head . lines

hexToBin c
  | ord c > ord '9' = hexToBin' $ 10 + ord c - ord 'A'
  | otherwise = hexToBin' $ ord c - ord '0'
  where
    hexToBin' x
      | x < 1 = reverse $ intToBin 4 x
      | x < 2 = reverse $ intToBin 3 x
      | x < 4 = reverse $ intToBin 2 x
      | x < 8 = reverse $ intToBin 1 x
      | otherwise = reverse $ intToBin 0 x
    intToBin n 0 = replicate n 0
    intToBin n x = (x `mod` 2) : intToBin n (x `div` 2)
