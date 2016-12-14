{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative ((<|>))
import Data.FileEmbed (embedStringFile)
import Control.Lens
import Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue as Deq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import Text.Trifecta

input :: String
input = $(embedStringFile "input.txt")

type Key = Integer

type Value = Integer

data Bot = Bot { _lo :: Maybe Value, _hi :: Maybe Value }
  deriving (Eq,Show,Generic)
makeLenses ''Bot

defaultBot :: Bot
defaultBot = Bot Nothing Nothing

type Output = HashMap Key [Value]

defaultOutput :: Output
defaultOutput = HM.empty

type Bots = HashMap Key Bot
defaultBots :: Bots
defaultBots = HM.empty

type Factory = (Bots, Output)

data Recipient = ToOutput | ToBot deriving (Eq,Show)

data Instruction = GoesTo { _goesValue :: Value, _goesKey :: Key }
                 | GivesTo { _givesKey :: Key
                           , _givesLoRecipient :: Recipient
                           , _givesLoKey :: Key
                           , _givesHiRecipient :: Recipient
                           , _givesHiKey :: Key }
    deriving (Eq,Show)
makeLenses ''Instruction

type Instructions = BankersDequeue Instruction

infixl 4 <$!>
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a

parseGoesTo :: Parser Instruction
parseGoesTo = GoesTo <$!>
              (string "value" *> spaces *> natural) <*>
              (string "goes to bot" *> spaces *> natural)

parseRecipient :: Parser Recipient
parseRecipient = (string "output" *> spaces >> return ToOutput) <|>
                 (string "bot" *> spaces >> return ToBot)

parseGivesTo :: Parser Instruction
parseGivesTo = GivesTo <$!>
               (string "bot " *> natural) <*>
               (string "gives low to" *> spaces *> parseRecipient) <*>
               natural <*>
               (string "and high to" *> spaces *> parseRecipient) <*>
               natural

parseInstructions :: Parser Instructions
parseInstructions = Deq.fromList <$!> many (parseGoesTo <|> parseGivesTo)

updateBot :: Value -> Bot -> Maybe Bot
updateBot v b@(Bot l h)
  | isNothing l && isNothing h   = Just $ b & lo ?~ v
  | isJust l && isJust h         = Nothing
  | isJust l && v <= l ^?! _Just = Just $ b & (lo ?~ v) . (hi .~ l)
  | isJust l && v > l ^?! _Just  = Just $ b & (hi ?~ v)
  | isJust h && v <= h ^?! _Just = Just $ b & (lo ?~ v)
  | isJust h && v > h ^?! _Just  = Just $ b & (lo .~ h) . (hi ?~ v)
  | otherwise                    = Nothing

botGivesOut :: Bot
            -> Lens' Bot (Maybe Value)
            -> Key
            -> Output
            -> Maybe (Bot, Output)
botGivesOut b l k os = do
  v <- b ^. l
  return (b & l .~ Nothing, os & at k . non [] %~ (v:))

botGivesBot :: Bot
            -> Lens' Bot (Maybe Value)
            -> Key
            -> Bots
            -> Maybe (Bot, Bots)
botGivesBot b l k bs = do
  v <- b ^. l
  c <- bs ^? at k . non defaultBot
  c' <- updateBot v c
  return (b & l .~ Nothing, bs & at k ?~ c')

attempt :: Instruction -> Factory -> Maybe Factory
attempt (GoesTo v k) (bs, os) = do
  b <- bs ^? at k . non defaultBot
  b' <- updateBot v b
  return (bs & at k ?~ b', os)
attempt (GivesTo k lR lK hR hK) (bs, os)
  | k == lK || k == hK = error "nonsensical instruction: key overlap"
  | lR == ToOutput && hR == ToOutput = do
    b <- bs ^. at k
    (b', os') <- botGivesOut b lo lK os
    (_, os'') <- botGivesOut b' hi hK os'
    return (bs & at k .~ Nothing, os'')
  | lR == ToOutput && hR == ToBot    = do
    b <- bs ^. at k
    (b', bs') <- botGivesBot b lo lK bs
    (_, os') <- botGivesOut b' hi hK os
    return (bs' & at k .~ Nothing, os')
  | lR == ToBot && hR == ToOutput    = do
    b <- bs ^. at k
    (b', os') <- botGivesOut b lo lK os
    (_, bs') <- botGivesBot b' hi hK bs
    return (bs' & at k .~ Nothing, os')
  | otherwise {- both ToBot -}       = do
    b <- bs ^. at k
    (b', bs') <- botGivesBot b lo lK bs
    (_, bs'') <- botGivesBot b' hi hK bs'
    return (bs'' & at k .~ Nothing, os)

-- TODO REMOVE
is :: Instructions
is = parseString parseInstructions mempty input ^?! _Success
ii :: Instruction
ii = is ^. to Deq.first ^?! _Just
ij :: Instruction
ij = is ^. to Deq.popFront ^?! _Just . _2 ^. to Deq.first ^?! _Just
bb :: Bot
bb = Bot (Just 17) Nothing

main :: IO ()
main = do
  let instructions = parseString parseInstructions mempty input ^?! _Success
  print . length $ instructions
