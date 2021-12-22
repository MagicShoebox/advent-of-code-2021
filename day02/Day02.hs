module Main (main) where

import Util.Advent (showResult, tbd)

main = showResult part1 part2

part1 input = horizontal * depth
  where
    Position horizontal depth = foldl navigate (Position 0 0) (commands input)

part2 = tbd

type Horizontal = Int

type Depth = Int

data Position = Position Horizontal Depth

data Command = Command Direction Int

data Direction = Forward | Down | Up

navigate (Position x y) (Command Forward d) = Position (x + d) y
navigate (Position x y) (Command Down d) = Position x (y + d)
navigate (Position x y) (Command Up d) = Position x (y - d)

commands input = map (parseCommand . words) (lines input)

parseCommand [direction, distance] = Command (parseDirection direction) (read distance)
parseCommand _ = error "Bad command"

parseDirection "forward" = Forward
parseDirection "down" = Down
parseDirection "up" = Up
parseDirection _ = error "Bad direction"
