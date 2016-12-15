{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.FileEmbed (embedStringFile)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue as Deq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust, isNothing)
import Data.Monoid ((<>), First(..))
import GHC.Generics (Generic)
import Text.Trifecta

input :: String
input = $(embedStringFile "input.txt")

type Key = Integer

type Value = Integer

data Bot = Bot { _lo :: Maybe Value, _hi :: Maybe Value }
  deriving (Eq,Show,Generic)
makeLenses ''Bot

inBot :: Value -> Bot -> Bool
inBot v b = or . concat $ [b ^.. lo . eq, b ^.. hi . eq]
  where eq = _Just . to (== v)

type Output = HashMap Key [Value]

type Bots = HashMap Key Bot

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

runTillDone :: StateT Factory IO ()
runTillDone = do
  miIs <- use $ instructions . to Deq.popFront
  if isNothing miIs
  then liftIO $ putStrLn "DONE"
  else do
    let (i, is) = miIs ^?! _Just
    liftIO . print =<< compares valueA valueB i <$> get
    mf' <- attempt i <$> get
    if isJust mf'
    then do
      liftIO . putStrLn $ "SUCCESS; ONE SHORTER " ++ (show $ length is)
      instructions .= is
      bots .= mf' ^?! _Just . bots
      runTillDone
    else do
      liftIO . putStrLn $ "FAIL; PUSHBACK " ++ (show $ length is)
      instructions .= Deq.pushBack is i
      runTillDone

botOrNew :: Factory -> Key -> Bot
botOrNew f k = f ^. bots . at k . non (Bot Nothing Nothing)

whenKey :: Key -> Bool -> Maybe Key
whenKey k p = if p then Just k else Nothing

compares :: Value -> Value -> Instruction -> Factory -> Maybe Key
compares x y (GoesTo v k) f = k `whenKey` p
  where
    b = f `botOrNew` k
    p = (x == v && y `inBot` b) || (y == v && x `inBot` b)
compares x y (GivesTo k lR lK hR hK) f
  | lR == ToBot && hR == ToBot = getFirst $ First mL <> First mH
  | lR == ToBot = mL
  | hR == ToBot = mH
  | otherwise = Nothing
  where
    b = f `botOrNew` k
    l = f `botOrNew` lK
    h = f `botOrNew` hK
    pl = (x `inBot` b && y `inBot` l) || (y `inBot` b && x `inBot` l)
    ph = (x `inBot` b && y `inBot` h) || (y `inBot` b && x `inBot` h)
    mL = lK `whenKey` pl
    mH = hK `whenKey` ph

updateBot :: Value -> Bot -> Bot
updateBot v b@(Bot l h)
  | isNothing l && isNothing h   = b & lo ?~ v
  | isJust l && isJust h         = b
  | isJust l && v <= l ^?! _Just = b & (lo ?~ v) . (hi .~ l)
  | isJust l && v > l ^?! _Just  = b & (hi ?~ v)
  | isJust h && v <= h ^?! _Just = b & (lo ?~ v)
  | isJust h && v > h ^?! _Just  = b & (lo .~ h) . (hi ?~ v)
  | otherwise                    = b

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
  c <- bs ^? at k1 . non (Bot Nothing Nothing)
  let v = b ^. l
  let c' = if isJust v then (updateBot (v ^?! _Just) c) else c
  let b' = if isJust v then (b & l .~ Nothing) else b
  return $! bs & (at k0 ?~ b') . (at k1 ?~ c')

botsAndOutput :: Factory -> (Bots, Output)
botsAndOutput = (^. to (id &&& id) . alongside bots output)

attempt :: Instruction -> Factory -> Maybe Factory
attempt (GoesTo v k) f = do
  let b = updateBot v $ f `botOrNew` k
  return $! f & bots . at k ?~ b
attempt (GivesTo k lR lK hR hK) f
  | k == lK || k == hK = error "nonsensical instruction: key overlap"
  | lR == ToOutput && hR == ToOutput = do
    let (bs, os) = botsAndOutput f
    (bs', os') <- botGivesOut k lo lK bs os
    (bs'', os'') <- botGivesOut k hi hK bs' os'
    return $! f & (bots .~ bs'') . (output .~ os'')
  | lR == ToOutput && hR == ToBot    = do
    let (bs, os) = botsAndOutput f
    bs' <- botGivesBot k lo lK bs
    (bs'', os') <- botGivesOut k hi hK bs' os
    return $! f & (bots .~ bs'') . (output .~ os')
  | lR == ToBot && hR == ToOutput    = do
    let (bs, os) = botsAndOutput f
    (bs', os') <- botGivesOut k lo lK bs os
    bs'' <- botGivesBot k hi hK bs'
    return $! f & (bots .~ bs'') . (output .~ os')
  | otherwise {- both ToBot -}       = do
    let bs = f ^. bots
    bs' <- botGivesBot k lo lK bs
    bs'' <- botGivesBot k hi hK bs'
    return $! f & bots .~ bs''

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
f, g :: Factory
f = mkFactory is
g = attempt ii f ^?! _Just
ii :: Instruction
ii = is ^. to Deq.first ^?! _Just
ij :: Instruction
ij = is ^. to Deq.popFront ^?! _Just . _2 ^. to Deq.first ^?! _Just
jj = ij{_givesKey = 209}
bb :: Bot
bb = Bot (Just 17) Nothing

main :: IO ()
main = do
  let inputIns = parseString parseInstructions mempty input ^?! _Success
  print . length $ inputIns
