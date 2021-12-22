module Main (main) where

import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = horizontal * depth
  where
    Position horizontal depth _ = foldl navigate (Position 0 0 0) (commands input)
    navigate (Position x y _) (Command Forward d) = Position (x + d) y 0
    navigate (Position x y _) (Command Down d) = Position x (y + d) 0
    navigate (Position x y _) (Command Up d) = Position x (y - d) 0

part2 input = horizontal * depth
  where
    Position horizontal depth _ = foldl navigate (Position 0 0 0) (commands input)
    navigate (Position x y a) (Command Forward d) = Position (x + d) (y + d * a) a
    navigate (Position x y a) (Command Down d) = Position x y (a + d)
    navigate (Position x y a) (Command Up d) = Position x y (a - d)

type Horizontal = Int

type Depth = Int

type Aim = Int

data Position = Position Horizontal Depth Aim

data Command = Command Direction Int

data Direction = Forward | Down | Up

commands input = map (parseCommand . words) (lines input)

parseCommand [direction, distance] = Command (parseDirection direction) (read distance)
parseCommand _ = error "Bad command"

parseDirection "forward" = Forward
parseDirection "down" = Down
parseDirection "up" = Up
parseDirection _ = error "Bad direction"
