{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.Trans.State (execState, get, put)
import Data.Array (Array, listArray, (!), (//))
import Data.Set (Set)
import qualified Data.Set as S (fromList, map, member, notMember, singleton, size, toList)
import Data.Tree (Tree (Node), foldTree, rootLabel, subForest)
import Debug.Trace (traceShow, traceShowId)
import Util.Advent (showResult, tbd)

type Reg = Char

data Op = Add | Mul | Div | Mod | Eql | Neq

data Instr = Input Int | Lit Int | Op Op | Agg (Set Int)

data State = State {inpIdx :: Int, regs :: Array Char (Tree Instr)}

instance Show Instr where
  show (Input x) = "s[" ++ show x ++ "]"
  show (Lit x) = show x
  show (Op op) = show op
  show (Agg xs) = "{" ++ show (S.size xs) ++ "}"

instance Show Op where
  show Add = "+"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Eql = "=="
  show Neq = "!="

main = showResult part1 part2

part1 input = draw $ regs ! 'z'
  where
    State {regs} = parseInput input

part2 = tbd

draw Node {rootLabel, subForest} = draw' subForest rootLabel
  where
    draw' [] root = show root
    draw' [left, right] root = "(" ++ draw left ++ " " ++ show root ++ " " ++ draw right ++ ")"
    draw' [child] root = show root
    draw' _ _ = undefined

lit x = Node (Lit x) []

inp x = Node (Input x) []

agg xs i = Node (Agg xs) [i]

left `add` right = optimize (Op Add) [left, right]

left `mul` right = optimize (Op Mul) [left, right]

left `div'` right = optimize (Op Div) [left, right]

left `mod'` right = optimize (Op Mod) [left, right]

left `neq` right = optimize (Op Neq) [left, right]

optimize (Op Mod) [Node (Op Add) [left, right], Node (Lit x) []] = optimize' (Op Mod) [(left `mod'` lit x) `add` (right `mod'` lit x), lit x]
optimize (Op Mod) [Node (Op Mul) [left, right], Node (Lit x) []] = optimize' (Op Mod) [(left `mod'` lit x) `mul` (right `mod'` lit x), lit x]
optimize instr subf = optimize' instr subf

optimize' (Input x) [] = agg (S.fromList [1 .. 9]) (inp x)
optimize' (Op Add) [left, Node (Lit 0) []] = left
optimize' (Op Add) [Node (Lit 0) [], right] = right
optimize' (Op Add) [Node (Lit x) [], Node (Lit y) []] = lit (x + y)
optimize' (Op Add) [Node (Agg xs) [i], Node (Lit y) []] = agg (S.map (+ y) xs) (i `add` lit y)
optimize' (Op Add) [Node (Agg xs) [i], Node (Agg ys) [j]]
  | (S.size xs < 1000 && S.size ys < 1000) || S.size ys == 1 = agg (S.fromList [x + y | x <- S.toList xs, y <- S.toList ys]) (i `add` j)
optimize' (Op Add) [Node (Op Add) [left, Node (Lit x) []], Node (Lit y) []] = left `add` lit (x + y)
optimize' (Op Add) [Node (Op Add) [left, Node (Lit x) []], Node (Op Add) [right, Node (Lit y) []]] = left `add` right `add` lit (x + y)
-- optimize' (Op Add) [Node (Op Mul) [left, Node (Lit x) []], Node (Op Mul) [right, Node (Lit y) []]]
--   | x == y = Node (Op Mul) [Node (Op Add) [left, right], lit x]
-- optimize' (Op Add) [Node (Op Mod) [left, Node (Lit x) []], Node (Op Mod) [right, Node (Lit y) []]]
--   | x == y = Node (Op Mod) [Node (Op Add) [left, right], lit x]
optimize' (Op Mul) [left, Node (Lit 0) []] = lit 0
optimize' (Op Mul) [Node (Lit 0) [], right] = lit 0
optimize' (Op Mul) [left, Node (Lit 1) []] = left
optimize' (Op Mul) [Node (Lit 1) [], right] = right
optimize' (Op Mul) [Node (Lit x) [], Node (Lit y) []] = lit (x * y)
optimize' (Op Mul) [Node (Agg xs) [i], Node (Lit y) []] = agg (S.map (* y) xs) (i `mul` lit y)
optimize' (Op Mul) [Node (Lit x) [], Node (Agg ys) [i]] = agg (S.map (* x) ys) (i `mul` lit x)
optimize' (Op Mul) [Node (Agg xs) [i], Node (Agg ys) [j]]
  | (S.size xs < 1000 && S.size ys < 1000) || S.size ys == 1 = agg (S.fromList [x * y | x <- S.toList xs, y <- S.toList ys]) (i `mul` j)
optimize' (Op Mul) [Node (Op Mul) [left, Node (Lit x) []], Node (Lit y) []] = left `mul` lit (x * y)
optimize' (Op Mul) [Node (Op Add) [left, right], Node (Lit x) []] = (left `mul` lit x) `add` (right `mul` lit x)
optimize' (Op Mul) [Node (Op Add) [left, right], Node (Agg xs) [i]] = (left `mul` agg xs i) `add` (right `mul` agg xs i)
optimize' (Op Div) [Node (Lit 0) [], right] = lit 0
optimize' (Op Div) [left, Node (Lit 1) []] = left
optimize' (Op Div) [Node (Lit x) [], Node (Lit y) []] = lit (x `div` y)
optimize' (Op Div) [Node (Agg xs) [i], Node (Lit y) []] = agg (S.map (`div` y) xs) (i `div'` lit y)
-- optimize' (Op Div) [Node (Input x) [], Node (Lit y) []]
--   | y > 9 = lit 0
optimize' (Op Div) [Node (Op Mul) [left, Node (Lit x) []], Node (Lit y) []]
  | x `mod` y == 0 = left `mul` lit (x `div` y)
optimize' (Op Div) [Node (Op Add) [left, right], Node (Lit x) []] = (left `div'` lit x) `add` (right `div'` lit x)
optimize' (Op Mod) [Node (Lit 0) [], right] = lit 0
optimize' (Op Mod) [left, Node (Lit 1) []] = lit 0
optimize' (Op Mod) [Node (Lit x) [], Node (Lit y) []] = lit (x `mod` y)
optimize' (Op Mod) [Node (Agg xs) [i], Node (Lit y) []] = agg (S.map (`mod` y) xs) (i `mod'` lit y)
-- optimize' (Op Mod) [Node (Input x) [], Node (Lit y) []]
--   | y > 9 = Node (Input x) []
-- optimize' (Op Mod) [Node (Op Add) [Node (Input x) [], Node (Lit y) []], Node (Lit z) []]
--   | z > y + 9 = Node (Input x) [] `add` lit y
optimize' (Op Mod) [Node (Op Neq) [left, right], Node (Lit x) []]
  | x > 2 = left `neq` right
optimize' (Op Eql) [Node (Op Eql) [left, right], Node (Lit 0) []] = left `neq` right
-- optimize' (Op Eql) [Node (Lit x) [], Node (Input _) []]
--   | x > 9 = lit 0
-- optimize' (Op Eql) [Node (Input _) [], Node (Lit x) []]
--   | x > 9 = lit 0
optimize' (Op Eql) [Node (Lit x) [], Node (Lit y) []] = lit (if x == y then 1 else 0)
-- optimize' (Op Neq) [Node (Lit x) [], Node (Input _) []]
--   | x > 9 = lit 1
-- optimize' (Op Neq) [Node (Input _) [], Node (Lit x) []]
--   | x > 9 = lit 1
optimize' (Op Neq) [Node (Lit x) [], Node (Lit y) []] = lit (if x /= y then 1 else 0)
optimize' (Op Neq) [Node (Lit x) [], Node (Agg ys) [i]]
  | x `S.notMember` ys = agg (S.singleton 1) (i `neq` lit x)
  | S.size ys == 1 = agg (S.singleton 0) (i `neq` lit x)
  | otherwise = agg (S.fromList [0, 1]) (i `neq` lit x)
optimize' (Op Neq) [Node (Agg xs) [i], Node (Agg ys) [j]]
  | not $ any (`S.member` ys) xs = agg (S.singleton 1) (i `neq` j)
  | S.size xs == 1 && S.size ys == 1 = agg (S.singleton 0) (i `neq` j)
  | otherwise = agg (S.fromList [0, 1]) (i `neq` j)
-- optimize' (Op Neq) [Node (Op Add) [Node (Input _) [], Node (Lit x) []], Node (Input _) []]
--   | x > 8 = lit 1
-- optimize' (Op Neq) [Node (Op Add) [Node (Input _) [], Node (Lit x) []], Node (Input _) []]
--   | x > 8 = lit 1
-- optimize' (Op Neq) [Node (Op Add) [_, Node (Lit x) []], Node (Input _) []]
--   | x > 9 = lit 1
optimize' instr subf = Node instr subf

parseInput input = execState instructions initial
  where
    instructions = mapM (parseInstruction . words) $ lines input
    initial = State {inpIdx = 0, regs = listArray ('w', 'z') (repeat $ lit 0)}

parseInstruction ["inp", reg : _] = do
  State {inpIdx, regs} <- get
  let idx' = inpIdx + 1
  let regs' = regs // [(reg, optimize (Input inpIdx) [])]
  put $ State idx' regs'
parseInstruction [op, reg : _, term] = do
  State {inpIdx, regs} <- get
  let reg' = optimize (Op $ parseOp op) [regs ! reg, parseTerm regs term]
  let regs' = regs // [(reg, reg')]
  put $ State inpIdx regs'
parseInstruction _ = undefined

parseTerm regs (t : ts)
  | t `elem` ['w' .. 'z'] = regs ! t
  | otherwise = lit $ read $ t : ts
parseTerm _ _ = undefined

parseOp "add" = Add
parseOp "mul" = Mul
parseOp "div" = Div
parseOp "mod" = Mod
parseOp "eql" = Eql
parseOp _ = undefined
