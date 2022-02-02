module Main (main) where

import Control.Monad.Trans.State (State, evalState, execState, get, gets, modify, put)
import Data.Array (Array, accum, elems, listArray, (!), (//))
import Data.Char (digitToInt)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, empty, fromList, intersection, member, singleton, size, toList, union)
import Debug.Trace (trace, traceShow, traceShowId)
import Util.Advent (showResult, tbd)

type Reg = Char

data Op = Add | Mul | Div | Mod | Eql

data Node = Node Term (Set Int)

data Term = Input Int | Literals | Instr Op Node Node

main = showResult part1 part2

part1 input = [trace (show idx ++ ":" ++ show val ++ ":") traceShowId $ check instructions idx val | idx <- [13, 12 .. 0], val <- [9, 8 .. 1]]
  where
    instructions = parseInput input

part2 = tbd

instance Show Node where
  show (Node (Input x) _) = "s[" ++ show x ++ "]"
  show (Node Literals xs) = show (toList xs)
  show (Node (Instr op left right) xs) = "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"

instance Show Op where
  show Add = "+"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Eql = "=="

check instructions index value = let (Node _ values) = check' instructions in 0 `member` values
  where
    check' (Node (Input x) values)
      | x == index = Node (Input x) (singleton value)
      | otherwise = Node (Input x) values
    check' (Node (Instr op left right) values) = optimize op (check' left) (check' right)
    check' literals = literals

optimize op (Node Literals left) (Node Literals right) = Node Literals $ combine op left right
optimize Add left@(Node _ leftValues) right@(Node _ rightValues)
  | all (== 0) leftValues = right
  | all (== 0) rightValues = left
optimize Mul left@(Node _ leftValues) right@(Node _ rightValues)
  | all (== 1) leftValues = right
  | all (== 1) rightValues = left
optimize Mul (Node _ leftValues) (Node _ rightValues)
  | all (== 0) leftValues = Node Literals (singleton 0)
  | all (== 0) rightValues = Node Literals (singleton 0)
optimize Div left (Node _ rightValues)
  | all (== 1) rightValues = left
optimize Div (Node _ leftValues) (Node _ rightValues)
  | maximum leftValues < minimum rightValues = Node Literals (singleton 0)
optimize Mod left@(Node _ leftValues) (Node _ rightValues)
  | maximum leftValues < minimum rightValues = left
optimize Eql (Node _ leftValues) (Node _ rightValues)
  | null (leftValues `intersection` rightValues) = Node Literals (singleton 0)
optimize op (Node left@Instr {} leftValues) (Node right@Instr {} rightValues) = Node (Instr op (Node left empty) (Node right empty)) (combine op leftValues rightValues)
optimize op (Node left@Instr {} leftValues) (Node right rightValues) = Node (Instr op (Node left empty) (Node right rightValues)) (combine op leftValues rightValues)
optimize op (Node left leftValues) (Node right@Instr {} rightValues) = Node (Instr op (Node left leftValues) (Node right empty)) (combine op leftValues rightValues)
optimize op (Node left leftValues) (Node right rightValues) = Node (Instr op (Node left leftValues) (Node right rightValues)) (combine op leftValues rightValues)

combine op left right = fromList [doOp op x y | x <- toList left, y <- toList right]

parseInput input = finalRegs ! 'z'
  where
    (_, finalRegs) = execState instructions initial
    instructions = mapM (parseInstruction . words) $ lines input
    initial = (0, listArray ('w', 'z') (repeat $ Node Literals $ singleton 0))

parseInstruction ["inp", reg : _] = do
  (idx, regs) <- get
  let values = fromList [1 .. 9]
  let term = Input idx
  put (idx + 1, regs // [(reg, Node term values)])
parseInstruction [op, reg : _, term@(t : _)] = do
  (idx, regs) <- get
  let isReg c = c == 'w' || c == 'x' || c == 'y' || c == 'z'
  let left = regs ! reg
  let right = if isReg t then regs ! t else Node Literals $ singleton (read term)
  let reg' = optimize (parseOp op) left right
  let regs' = regs // [(reg, reg')]
  put (idx, regs')
parseInstruction _ = undefined

parseOp "add" = Add
parseOp "mul" = Mul
parseOp "div" = Div
parseOp "mod" = Mod
parseOp "eql" = Eql
parseOp _ = undefined

doOp Add = (+)
doOp Mul = (*)
doOp Div = div
doOp Mod = mod
doOp Eql = \a b -> if a == b then 1 else 0

headMaybe [] = Nothing
headMaybe (x : _) = Just x
