{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative ((<|>))
import Data.FileEmbed (embedStringFile)
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
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

hasValue :: Value -> Bot -> Bool
hasValue v b = or . concat $ [b ^.. lo . eq, b ^.. hi . eq]
  where eq = _Just . to (== v)

defaultBot :: Bot
defaultBot = Bot Nothing Nothing

type Output = HashMap Key [Value]

defaultOutput :: Output
defaultOutput = HM.empty

type Bots = HashMap Key Bot
defaultBots :: Bots
defaultBots = HM.empty

-- type Factory = (Bots, Output)

data Recipient = ToOutput | ToBot deriving (Eq,Show)

data Instruction = GoesTo { _goesValue :: Value, _goesKey :: Key }
                 | GivesTo { _givesKey :: Key
                           , _givesLoRecipient :: Recipient
                           , _givesLoKey :: Key
                           , _givesHiRecipient :: Recipient
                           , _givesHiKey :: Key }
    deriving (Eq,Show)
makeLenses ''Instruction

isGoesTo :: Instruction -> Bool
isGoesTo GoesTo{} = True
isGoesTo _ = False

isGivesTo :: Instruction -> Bool
isGivesTo GivesTo{} = True
isGivesTo _ = False

type Instructions = BankersDequeue Instruction

data Factory = Factory { _bots :: Bots
                       , _output :: Output
                       , _instructions :: Instructions }
  deriving (Eq,Show)
makeLenses ''Factory

mkFactory :: Instructions -> Factory
mkFactory is =
  Factory { _bots = HM.empty, _output = HM.empty, _instructions = is }

valueA, valueB :: Value
valueA = 61
valueB = 17

attemptS :: StateT Factory IO ()
attemptS = do
  miIs <- use (instructions . to Deq.popFront)
  if isNothing miIs
  then lift $ putStrLn "DONE"
  else do
    let (i, is) = miIs ^?! _Just
    lift . putStrLn $ "Attempting " ++ show i
    mf' <- attempt i <$> get
    if isJust mf'
    then do
      lift $ putStrLn "success; continuing with shorter instructions"
      instructions .= is
      bots .= mf' ^?! _Just . bots
    else do
      lift $ putStrLn "failure; pushing instruction on back of queue"
      instructions .= Deq.pushBack is i

compares :: Value -> Value -> Instruction -> Factory -> Bool
compares a b (GoesTo v k) f =
  (a == v && fHasValue b) || (b == v && fHasValue a)
  where fHasValue x = or $ f ^.. bots . at k . _Just . to (hasValue x)
compares a b (GivesTo k lR lK hR hK) f = undefined

updateBot :: Value -> Bot -> Maybe Bot
updateBot v b@(Bot l h)
  | isNothing l && isNothing h   = Just $ b & lo ?~ v
  | isJust l && isJust h         = Nothing
  | isJust l && v <= l ^?! _Just = Just $ b & (lo ?~ v) . (hi .~ l)
  | isJust l && v > l ^?! _Just  = Just $ b & (hi ?~ v)
  | isJust h && v <= h ^?! _Just = Just $ b & (lo ?~ v)
  | isJust h && v > h ^?! _Just  = Just $ b & (lo .~ h) . (hi ?~ v)
  | otherwise                    = Nothing

botGivesOut :: Key
            -> Lens' Bot (Maybe Value)
            -> Key
            -> Bots
            -> Output
            -> Maybe (Bots, Output)
botGivesOut k0 l k1 bs os = do
  b <- bs ^. at k0
  v <- b ^. l
  let b' = b & l .~ Nothing
  return (bs & at k0 ?~ b', os & at k1 . non [] %~ (v:))

botGivesBot :: Key
            -> Lens' Bot (Maybe Value)
            -> Key
            -> Bots
            -> Maybe Bots
botGivesBot k0 l k1 bs = do
  b <- bs ^. at k0
  v <- b ^. l
  c <- bs ^? at k1 . non defaultBot
  c' <- updateBot v c
  return $! bs & (at k0 .~ Nothing) . (at k1 ?~ c')

attempt :: Instruction -> Factory -> Maybe Factory
attempt (GoesTo v k) f = do
  let bs = f ^. bots
  b <- bs ^? at k . non defaultBot
  b' <- updateBot v b
  return $! f & bots .~ (bs & at k ?~ b')
attempt (GivesTo k lR lK hR hK) f
  | k == lK || k == hK = error "nonsensical instruction: key overlap"
  | lR == ToOutput && hR == ToOutput = do
    let (bs, os) = (f, f) ^. alongside bots output
    (bs', os') <- botGivesOut k lo lK bs os
    (bs'', os'') <- botGivesOut k hi hK bs' os'
    return $! f & (bots .~ bs'') . (output .~ os'')
  | lR == ToOutput && hR == ToBot    = do
    let (bs, os) = (f, f) ^. alongside bots output
    bs' <- botGivesBot k lo lK bs
    (bs'', os') <- botGivesOut k hi hK bs' os
    return $! f & (bots .~ bs'') . (output .~ os')
  | lR == ToBot && hR == ToOutput    = do
    let (bs, os) = (f, f) ^. alongside bots output
    (bs', os') <- botGivesOut k lo lK bs os
    bs'' <- botGivesBot k hi hK bs
    return $! f & (bots .~ bs'') . (output .~ os')
  | otherwise {- both ToBot -}       = do
    let bs = f ^. bots
    bs' <- botGivesBot k lo lK bs
    bs'' <- botGivesBot k hi hK bs'
    return $! f & bots .~ bs''

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


-- TODO REMOVE
is :: Instructions
is = parseString parseInstructions mempty input ^?! _Success
f :: Factory
f = mkFactory is
ii :: Instruction
ii = is ^. to Deq.first ^?! _Just
ij :: Instruction
ij = is ^. to Deq.popFront ^?! _Just . _2 ^. to Deq.first ^?! _Just
bb :: Bot
bb = Bot (Just 17) Nothing

main :: IO ()
main = do
  let inputIns = parseString parseInstructions mempty input ^?! _Success
  print . length $ inputIns
