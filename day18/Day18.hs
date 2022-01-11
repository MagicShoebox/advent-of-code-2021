module Main (main) where

import Data.Char (isDigit)
import Data.Tree (Tree (Node), foldTree)
import Debug.Trace (traceShow, traceShowId)
import Text.ParserCombinators.ReadP (ReadP, char, choice, many1, readP_to_S, satisfy)
import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = magnitude $ foldl1 (\a b -> reduce $ add a b) $ parseInput input

part2 = tbd

add x y = Node 0 [foldTree incrementDepth x, foldTree incrementDepth y]
  where
    incrementDepth x [] = Node x []
    incrementDepth x xs = Node (x + 1) xs

reduce = until (foldTree reduced) (split . explode)
  where
    reduced x [] = x < 10
    reduced n xs = n < 4 && and xs

explode t = let (_, exploded, _) = explode' t in exploded
  where
    explode' (Node x []) = (0, Node x [], 0)
    explode' (Node 4 [Node left [], Node right []]) = (left, Node 0 [], right)
    explode' (Node n [left, right]) =
      let (ll, lx, lr) = explode' left
          (rl, rx, rr) = explode' (addLeft right lr)
       in (ll, Node n [addRight lx rl, rx], rr)
    explode' _ = undefined
    addLeft (Node x []) l = Node (x + l) []
    addLeft (Node n [left, right]) l = Node n [addLeft left l, right]
    addLeft _ _ = undefined
    addRight (Node x []) r = Node (x + r) []
    addRight (Node n [left, right]) r = Node n [left, addRight right r]
    addRight _ _ = undefined

split t = either id id $ split' t
  where
    split' (Node x [])
      | x < 10 = Right $ Node x []
      | otherwise =
        let left = floor $ fromIntegral x / 2
            right = ceiling $ fromIntegral x / 2
         in Left $ Node (-1) [Node left [], Node right []]
    split' (Node n [left, right]) = case (split' left, split' right) of
      (Left (Node _ xs), _) -> Left $ Node n [Node (n + 1) xs, right]
      (_, Left (Node _ xs)) -> Left $ Node n [left, Node (n + 1) xs]
      (_, _) -> Right $ Node n [left, right]
    split' _ = undefined

magnitude (Node x []) = x
magnitude (Node _ [left, right]) = 3 * magnitude left + 2 * magnitude right
magnitude _ = undefined

parseInput = map (fst . last . readP_to_S (treeP 0)) . lines
  where
    nodeP = do
      cs <- many1 (satisfy isDigit)
      return $ Node (read cs) [] :: ReadP (Tree Int)
    treeP n = do
      char '['
      left <- choice [nodeP, treeP (n + 1)]
      char ','
      right <- choice [nodeP, treeP (n + 1)]
      char ']'
      return $ Node n [left, right]

display (Node x []) = show x
display (Node _ [left, right]) = "[" ++ display left ++ "," ++ display right ++ "]"
display _ = undefined
