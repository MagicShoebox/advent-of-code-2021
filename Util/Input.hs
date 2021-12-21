{-# LANGUAGE LambdaCase #-}

module Util.Input (putResult, showResult) where

import System.Environment (getArgs, getProgName)

showResult f = putResult $ show . f

putResult f =
  getArgs >>= parseArgs >>= \case
    Left progName -> putStrLn $ usage progName
    Right input -> putStrLn $ f input

parseArgs [] = Right <$> getContents
parseArgs ["-"] = Right <$> getContents
parseArgs [arg] = Right <$> readFile arg
parseArgs _ = Left <$> getProgName

usage name = "Usage: " ++ name ++ " [input.txt]\nIf no argument or argument is \"-\", read from stdin."
