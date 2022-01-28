module Main (main) where

import Control.Monad.Trans.State (State, evalState, gets, modify)
import Data.Array (Array, accum, elems, listArray, (!), (//))
import Data.Char (digitToInt)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, empty, member, singleton, union)
import Debug.Trace (trace, traceShow, traceShowId)
import Util.Advent (showResult, tbd)

type Reg = Char

type Op = Int -> Int -> Int

data Instr = Input Reg | OpReg Op Reg Reg | OpLit Op Reg Int | End

main = showResult part1 part2

part1 input = maximumValid instructions
  where
    instructions = parseInput input

part2 = tbd

maximumValid instructions = evalState (execute initialRegs 0) cache
  where
    initialRegs = listArray ('w', 'z') (repeat 0)
    cache = fmap (const empty) instructions
    execute regs n = do
      cached <- gets (! n)
      if (r1, r2, r3, r4) `member` cached
        then return Nothing
        else do
          result <- evaluate (instructions ! n)
          modify (\s -> accum union s [(n, singleton (r1, r2, r3, r4))])
          return result
      where
        [r1, r2, r3, r4] = elems regs
        evaluate (OpReg op d s) = execute (regs // [(d, op (regs ! d) (regs ! s))]) (n + 1)
        evaluate (OpLit op d l) = execute (regs // [(d, op (regs ! d) l)]) (n + 1)
        evaluate (Input r) = headMaybe . catMaybes <$> mapM (\i -> fmap (i :) <$> execute (regs // [(r, if n < 80 then trace (show n ++ ":" ++ show i) i else i)]) (n + 1)) [9, 8 .. 1]
        evaluate End = return $ if regs ! 'z' == 0 then Just [] else Nothing

parseInput input = listArray (0, length lines') instructions
  where
    lines' = lines input
    instructions = map (parseInstruction . words) lines' ++ [End]
    parseInstruction ["inp", reg : _] = Input reg
    parseInstruction [op, reg : _, term@(t : _)]
      | t `elem` ['w' .. 'z'] = OpReg (parseOp op) reg t
      | otherwise = OpLit (parseOp op) reg (read term)
    parseInstruction _ = undefined
    parseOp "add" = (+)
    parseOp "mul" = (*)
    parseOp "div" = div
    parseOp "mod" = mod
    parseOp "eql" = \a b -> if a == b then 1 else 0
    parseOp _ = undefined

headMaybe [] = Nothing
headMaybe (x : _) = Just x
