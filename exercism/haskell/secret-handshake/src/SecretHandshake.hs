module SecretHandshake (handshake) where

import Data.Bits  (bit, testBit, (.&.))
import Data.Bool  (bool)
import Data.Maybe (catMaybes)

handshake :: Int -> [String]
handshake n = bool id reverse (testBit n 4) steps
  where
    steps = concatMap (\i -> bool [] [table !! i] (testBit n i)) [0..3]
    table = ["wink", "double blink", "close your eyes", "jump"]
