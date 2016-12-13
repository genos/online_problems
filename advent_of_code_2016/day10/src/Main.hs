{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Text.Trifecta
import Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue as Deq
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.FileEmbed (embedStringFile)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

input :: String
input = $(embedStringFile "input.txt")

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
type Output = HashMap Key [Value]

data Bot = Bot { _lo :: Maybe Value , _hi :: Maybe Value }
  deriving (Eq, Show, Generic)
instance Hashable Bot

data Recip = O | B deriving (Eq, Show)

valueGoesTo :: Value -> Key -> Bots -> (Bool, Bots)
valueGoesTo v k bs = undefined

botGives :: Key -> (Recip, Key) -> (Recip, Key) -> Bots -> Output -> (Bool, Bots, Output)
botGives k (O, lK) (O, hK) _ os
  | otherwise = undefined
botGives k (B, lK) (O, hK) bs os
  | otherwise = undefined
botGives k (O, lK) (B, hK) bs os
  | otherwise = undefined
botGives k (B, lK) (B, hK) bs _
  | otherwise = undefined

data Instruction = ValueGoesTo Value Key
                 | GivesTo { _key :: Key
                           , _lR :: Recip
                           , _lK :: Key
                           , _hR :: Recip
                           , _hK :: Key }
type Instructions = BankersDequeue Instruction

attempt :: Instruction -> Bots -> Output -> (Bool, Bots, Output)
attempt (ValueGoesTo v k) bs os = (ok, bs', os)
  where (ok, bs') = valueGoesTo v k bs
attempt (GivesTo k lR lK hR hK) bs os = (ok, bs', os')
  where (ok, bs', os') = botGives k (lR, lK) (hR, hK) bs os

runTillEmpty :: (Bots, Output) -> Instructions -> (Bots, Output)
runTillEmpty (bs, os) is | Deq.null is = (bs, os)
                         | otherwise   = runTillEmpty (bs', os') is'
  where
    (ok, bs', os') = attempt h bs os
    (h, t)         = fromJust $! Deq.popFront is
    is'            = if ok then t else Deq.pushBack t h

pValueGoesTo :: Parser Instruction
pValueGoesTo = do
  _ <- string "value "
  v <- natural
  _ <- string "goes to bot "
  k <- natural
  return $! ValueGoesTo v k

parseRecip :: Parser Recip
parseRecip = do
  s <- string "output" <|> string "bot"
  _ <- spaces
  return $! if head s == 'o' then O else B

pGivesTo :: Parser Instruction
pGivesTo = do
  _ <- string "bot "
  k <- natural
  _ <- string "gives low to "
  lR <- parseRecip
  lK <- natural
  _ <- string "and high to "
  hR <- parseRecip
  hK <- natural
  return $! GivesTo k lR lK hR hK

instructions :: Parser Instructions
instructions = Deq.fromList <$> many (pValueGoesTo <|> pGivesTo)

main :: IO ()
main = do
  let is = parseString instructions mempty input
  print . fmap length $ is
  print . fmap (runTillEmpty (Map.empty, Map.empty)) $ is
