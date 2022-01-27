{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.Trans.State (execState, get, put)
import Data.Array (Array)
import qualified Data.Array as A (listArray, (!), (//))
import Data.Map (Map)
import qualified Data.Map as M (fromList, fromListWith, singleton, size, toList, unionWith, (!))
import Data.Set (Set)
import qualified Data.Set as S (empty, fromList, null, singleton, size, toList, union)
import Data.Tree (Tree (Node), foldTree, rootLabel, subForest)
import Debug.Trace (traceShow, traceShowId)
import Util.Advent (showResult, tbd)

type Reg = Char

type Input = Int

type Constraints = Map Input Int

type Values = Map Int (Set Constraints)

data State = State {inpIdx :: Int, regs :: Array Char Values}

main = showResult part1 part2

part1 input = (regs A.! 'z') M.! 0
  where
    State {regs} = parseInput input

part2 = tbd

combine op left right = traceShow (M.size left, M.size right) M.fromListWith S.union [(v1 `op` v2, combine' c1 c2) | (v1, c1) <- M.toList left, (v2, c2) <- M.toList right]
  where
    foo s1 s2 = undefined
    combine' cs1 cs2
      | S.null cs1 = cs2
      | S.null cs2 = cs1
      | otherwise = S.fromList [M.unionWith (\a b -> undefined) c1 c2 | c1 <- S.toList cs1, c2 <- S.toList cs2]

parseInput input = execState instructions initial
  where
    instructions = mapM (\(a, b) -> parseInstruction a (words b)) $ zip [0 ..] $ lines input
    initial = State {inpIdx = 0, regs = A.listArray ('w', 'z') (repeat $ M.singleton 0 S.empty)}

parseInstruction n ["inp", reg : _] = do
  State {inpIdx, regs} <- get
  let idx' = inpIdx + 1
  let values = M.fromList [(v, S.singleton $ M.singleton inpIdx v) | v <- [1 .. 9]]
  let regs' = regs A.// [(reg, values)]
  put $ State idx' regs'
parseInstruction n [op, reg : _, term@(t : _)] = do
  State {inpIdx, regs} <- get
  let isReg c = c == 'w' || c == 'x' || c == 'y' || c == 'z'
  let term' = if isReg t then regs A.! t else M.singleton (read term) S.empty
  let reg' = traceShow n combine (parseOp op) (regs A.! reg) term'
  let regs' = regs A.// [(reg, reg')]
  put $ State inpIdx regs'
parseInstruction _ _ = undefined

parseOp "add" = (+)
parseOp "mul" = (*)
parseOp "div" = div
parseOp "mod" = mod
parseOp "eql" = eql
parseOp _ = undefined

eql x y = if x == y then 1 else 0
