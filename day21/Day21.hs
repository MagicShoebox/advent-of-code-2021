module Main (main) where

import Control.Arrow (first, (&&&))
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Map (adjust, fromListWith, keys, partitionWithKey, singleton, toList, unionWith)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace (trace, traceShow, traceShowId)
import Util.Advent (showResult, tbd)
import Util.List (frequency)

data Game
  = Player1Win
  | Player2Win
  | Active {p1score :: Int, p2score :: Int, p1position :: Int, p2position :: Int}
  deriving (Eq, Ord, Show)

main = showResult part1 part2

part1 input = losingScore * (rolls * 6 + 3)
  where
    rolls = uncurry min $ both (subtract 1 . fromJust . findIndex (>= 1000)) (p1scores, p2scores)
    losingScore = min (p1scores !! rolls) (p2scores !! rolls)
    p1scores = scores $ positions p1movements p1start
    p2scores = scores $ positions p2movements p2start
    scores = scanl (+) 0 . map score . tail
    positions movements start = scanl move start movements
    (p1movements, p2movements) = both cycle ([6, 4, 2, 0, 8], [5, 3, 1, 9, 7])
    (p1start, p2start) = parseInput input

-- P1Wins overcounted by factor of 27 because branch precalculates P2's rolls
part2 input = maximum $ map snd $ toList $ adjust (`div` 27) Player1Win $ until (not . any isActive . keys) branch games
  where
    games = singleton (Active 0 0 p1start p2start) 1
    (p1start, p2start) = parseInput input

branch games = unionWith (+) wins $ fromListWith (+) games'
  where
    (actives, wins) = partitionWithKey (\k _ -> isActive k) games
    games' = [(turn old mv, gf * mf) | (old, gf) <- toList actives, (mv, mf) <- movements]
    movements = [((p1m, p2m), p1f * p2f) | (p1m, p1f) <- movement, (p2m, p2f) <- movement]
    movement = frequency [a + b + c | a <- [1 .. 3], b <- [1 .. 3], c <- [1 .. 3]]

turn (Active p1s p2s p1p p2p) (p1m, p2m)
  | p1s + score p1p' >= 21 = Player1Win
  | p2s + score p2p' >= 21 = Player2Win
  | otherwise = Active (p1s + score p1p') (p2s + score p2p') p1p' p2p'
  where
    (p1p', p2p') = (move p1p p1m, move p2p p2m)
turn _ _ = undefined

isActive Player1Win = False
isActive Player2Win = False
isActive _ = True

score 0 = 10
score x = x

move a b = (a + b) `mod` 10

both f (a, b) = (f a, f b)

parseInput input = both (digitToInt . last) $ head &&& head . tail $ lines input
