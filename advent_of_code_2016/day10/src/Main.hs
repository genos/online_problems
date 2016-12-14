{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Applicative ((<|>))
import Data.FileEmbed (embedStringFile)
import Control.Lens
import Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue as Deq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HS
import GHC.Generics (Generic)
import Text.Trifecta

type Key = Integer

type Value = Integer

data Bot = Bot
    { _lo :: Maybe Value
    , _hi :: Maybe Value
    } deriving (Eq,Show,Generic)

makeLenses ''Bot

type Output = HashMap Key [Value]

type Swarm = HashMap Key Bot

data Recipient
    = ToOutput
    | ToBot
    deriving (Eq,Show)

input :: String
input = $(embedStringFile "input.txt")

data Instruction
    = GoesTo Value
             Key
    | GivesTo Key
              (Recipient, Key)
              (Recipient, Key)
    deriving ((Show))

type Instructions = BankersDequeue Instruction

infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a

parseGoesTo :: Parser Instruction
parseGoesTo = GoesTo <$!>
              (string "value " *> natural) <*>
              (string "goes to bot " *> natural)

parseRecipKey :: Parser (Recipient, Key)
parseRecipKey = (,) <$!>
                ((string "output " >> return ToOutput) <|>
                 (string "bot " >> return ToBot)) <*>
                natural

parseGivesTo :: Parser Instruction
parseGivesTo = GivesTo <$!>
               (string "bot " *> natural) <*>
               (string "gives low to " *> parseRecipKey) <*>
               (string "and high to " *> parseRecipKey)

parseInstructions :: Parser Instructions
parseInstructions = Deq.fromList <$> many (parseGoesTo <|> parseGivesTo)

main :: IO ()
main = print . length $ input
