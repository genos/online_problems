{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text
import Data.Char (isLetter)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T

data Pulse = Low | High deriving (Eq, Show)
data OnOff = On | Off deriving (Eq, Show)
data Module = Broadcast | FlipFlop OnOff | Conjunction (Map Text Pulse) | Receiver (Int, Int) deriving (Show)
type Circuit = Map Text (Module, [Text])

readCircuit :: Text -> Circuit
readCircuit = either (error "bad parse") (addReceivers . fillInConj . M.fromList) . parseOnly ((module_ `sepBy1'` "\n") <* endOfInput)
  where
    module_ = choice [broadcast, flipflop, conjunction]
    broadcast = ("broadcaster",) . (Broadcast,) <$> ("broadcaster -> " *> names)
    flipflop = (,) <$> ("%" *> name) <*> ((FlipFlop Off,) <$> (" -> " *> names))
    conjunction = (,) <$> ("&" *> name) <*> ((Conjunction mempty,) <$> (" -> " *> names))
    names = name `sepBy'` ", "
    name = takeTill (not . isLetter)
    fillInConj circ = foldl' go circ (M.assocs $ M.filter (\(m, _ns) -> isConj m) circ)
      where
        go c (n, (Conjunction _, ns)) =
            let pulses = M.fromList . fmap (,Low) . M.keys $ M.filter (\(_, ns') -> n `elem` ns') c
             in M.insert n (Conjunction pulses, ns) c
        go c _ = c
    isConj = \case Conjunction _ -> True; _ -> False
    addReceivers circ = foldl' go circ (concatMap snd $ M.elems circ)
      where
        go c n
            | n `M.member` c = c
            | otherwise = M.insert n (Receiver (0, 0), []) c

tick :: Text -> Pulse -> Module -> (Module, Maybe Pulse)
tick _ p Broadcast = (Broadcast, Just p)
tick _ High f@(FlipFlop _) = (f, Nothing)
tick _ Low (FlipFlop o) = (FlipFlop o', Just p)
  where
    o' = if o == On then Off else On
    p = if o == On then Low else High
tick n p (Conjunction ins) = (Conjunction ins', Just p')
  where
    ins' = ins & at n ?~ p
    p' = if all (== High) (M.elems ins') then Low else High
tick _ p (Receiver (l, h)) = (Receiver $ (l, h) & (if p == Low then _1 else _2) +~ 1, Nothing)

type Action = (Text, Pulse, Text)
type State = (Seq Action, Circuit, (Int, Int))

step :: State -> State
step state@(S.Empty, _, _) = state
step ((src, pulse, dst) :<| actions, c, counts) = (actions', c', counts')
  where
    (m, outs) = c M.! dst
    (m', mp) = tick src pulse m
    c' = c & at dst ?~ (m', outs)
    todo = case mp of
        Nothing -> []
        Just p -> (dst,p,) <$> outs
    counts' = counts & (if pulse == Low then _1 else _2) +~ 1
    actions' = actions S.>< S.fromList todo

start :: Circuit -> State
start = (S.singleton ("button", Low, "broadcaster"),,(0, 0))

run :: Circuit -> Int -> (Int, Int)
run circ = (^. _2) . foldl' f (circ, (0, 0)) . enumFromTo 1
  where
    f (c, (l, h)) _ = let (_, c', (l', h')) = go $ start c in (c', (l + l', h + h'))
    go s = let s' = step s in if s' ^. _1 & S.null then s' else go s'

part1 :: Circuit -> Int
part1 = productOf each . (`run` 1000)

main :: IO ()
main = do
    input <- readCircuit <$> T.readFile "input.txt"
    print $ part1 input
