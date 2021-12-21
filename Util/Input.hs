{-# LANGUAGE LambdaCase #-}

module Util.Input (withInput) where

import System.Environment (getArgs, getProgName)

withInput f =
  getArgs >>= parseArgs >>= \case
    Left progName -> putStrLn $ usage progName
    Right input -> f input

parseArgs [] = Right <$> getContents
parseArgs ["-"] = Right <$> getContents
parseArgs [arg] = Right <$> readFile arg
parseArgs _ = Left <$> getProgName

usage name = "Usage: " ++ name ++ " [input.txt]\nIf no argument or argument is \"-\", read from stdin."
