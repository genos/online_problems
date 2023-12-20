{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.IO qualified as T

data Pulse = Low | High
data OnOff = On | Off
data Module = Broadcast | FlipFlop OnOff | Conjunction (Map Text Pulse)
type Circuit = Map Text (Module, [Text])

main :: IO ()
main = T.putStrLn "Day 20"
