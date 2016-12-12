{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Text.Trifecta
import Data.Hashable (Hashable)
import Data.List (isPrefixOf, sortOn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.FileEmbed (embedStringFile)
import GHC.Generics (Generic)

input :: String
input = $(embedStringFile "input.txt")

sortInput :: [String] -> [String]
sortInput = reverse . sortOn ("value" `isPrefixOf`)

test1Input :: [String]
test1Input =
    [ "value 5 goes to bot 2"
    , "bot 2 gives low to bot 1 and high to bot 0"
    , "value 3 goes to bot 1"
    , "bot 1 gives low to output 1 and high to bot 0"
    , "bot 0 gives low to output 2 and high to output 0"
    , "value 2 goes to bot 2"]

type Key = Integer

type Value = Integer

type Bots = HashMap Key Bot

type Bins = HashMap Key [Value]

data Bot = Bot
    { _lo :: Maybe Value
    , _hi :: Maybe Value
    } deriving (Eq,Show,Generic)

botGets :: Value -> Bot -> Bot
botGets v (Bot Nothing Nothing) = Bot Nothing (Just v)
botGets v (Bot Nothing (Just h))
  | v <= h = Bot (Just v) (Just h)
  | otherwise = Bot (Just h) (Just v)
botGets v (Bot (Just l) Nothing)
  | v <= l = Bot (Just v) (Just l)
  | otherwise = Bot (Just l) (Just v)
botGets _ _ = error "Bot Full"

instance Hashable Bot

output :: Bins
output = HM.empty

bots :: Bots
bots = HM.empty

valueGoesTo :: (Value, Key) -> Bots -> Bots
valueGoesTo (v, k) = HM.adjust (botGets v) k

valueLine :: Parser (Value, Key)
valueLine = do
    _ <- string "value "
    v <- natural
    _ <- string "goes to bot "
    k <- natural
    return $! (v, k)

main :: IO ()
main = print . length $ input
