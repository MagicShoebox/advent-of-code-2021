{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.Trans.State (execState, get, modify, put)
import Data.Array (Array, listArray, (!), (//))
import Data.Char (digitToInt)
import Debug.Trace (traceShow, traceShowId)
import Util.Advent (showResult, tbd)

type Reg = Char

type Op = Int -> Int -> Int

data Instr = Input Reg | OpReg Op Reg Reg | OpLit Op Reg Int

data State = State {input :: String, regs :: Array Char Int} deriving (Show)

main = showResult part1 part2

part1 input = until (validate instructions) (traceShowId . decrement) 99999999999999
  where
    decrement x = until (notElem '0' . show) (subtract 1) (x -1)
    instructions = parseInput input

part2 = tbd

validate instructions model = regs ! 'z' == 0
  where
    State {regs} = execState (execute instructions) initial
    initial = State {input = show model, regs = listArray ('w', 'z') (repeat 0)}

execute = mapM (modify . exec)
  where
    exec (Input r) State {input, regs} = let (i : is) = input in State is (regs // [(r, digitToInt i)])
    exec (OpReg op d s) State {input, regs} = State input (regs // [(d, op (regs ! d) (regs ! s))])
    exec (OpLit op d l) State {input, regs} = State input (regs // [(d, op (regs ! d) l)])

parseInput input = map (parseInstruction . words) $ lines input
  where
    parseInstruction ["inp", reg : _] = Input reg
    parseInstruction [op, reg : _, t : ts]
      | t `elem` ['w' .. 'z'] = OpReg (parseOp op) reg t
      | otherwise = OpLit (parseOp op) reg (read $ t : ts)
    parseInstruction _ = undefined
    parseOp "add" = (+)
    parseOp "mul" = (*)
    parseOp "div" = div
    parseOp "mod" = mod
    parseOp "eql" = \a b -> if a == b then 1 else 0
    parseOp _ = undefined
