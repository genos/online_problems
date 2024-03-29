{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Attoparsec.Text
import Data.Char (isLetter)
import Data.Foldable (foldl', traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T

data Pulse = Low | High deriving (Eq, Show)
data OnOff = On | Off deriving (Eq, Show)
data Module = Broadcast | FlipFlop !OnOff | Conjunction !(Map Text Pulse) | Sink
type Circuit = Map Text (Module, [Text])

isConj :: Module -> Bool
isConj = \case Conjunction _ -> True; _otherMod -> False

readCircuit :: Text -> Circuit
readCircuit =
    either (error "bad parse") (addSinks . fillInConj . M.fromList)
        . parseOnly ((module_ `sepBy1'` "\n") <* endOfInput)
  where
    module_ = choice [broadcast, flipflop, conjunction]
    broadcast = ("broadcaster",) . (Broadcast,) <$> ("broadcaster -> " *> names)
    flipflop = (,) <$> ("%" *> name) <*> ((FlipFlop Off,) <$> (" -> " *> names))
    conjunction = (,) <$> ("&" *> name) <*> ((Conjunction mempty,) <$> (" -> " *> names))
    names = name `sepBy'` ", "
    name = takeTill (not . isLetter)
    fillInConj circ = foldl' f circ (M.assocs $ M.filter (\(m, _ns) -> isConj m) circ)
      where
        f c (n, (Conjunction _, ns)) =
            let pulses = M.fromList . fmap (,Low) . M.keys $ M.filter (\(_, ns') -> n `elem` ns') c
             in c & at n ?~ (Conjunction pulses, ns)
        f c _ = c
    addSinks circ = foldl' f circ (concatMap snd $ M.elems circ)
      where
        f c n = if n `M.member` c then c else c & at n ?~ (Sink, [])

tick :: Text -> Pulse -> Module -> (Module, Maybe Pulse)
tick _ p Broadcast = (Broadcast, Just p)
tick _ Low (FlipFlop o) = (FlipFlop o', Just p)
  where
    o' = if o == On then Off else On
    p = if o == On then Low else High
tick n p (Conjunction ins) = (Conjunction ins', Just p')
  where
    ins' = ins & at n ?~ p
    p' = if all (== High) (M.elems ins') then Low else High
tick _ _ f = (f, Nothing)

type Action = (Text, Pulse, Text)
type Meter m = Action -> m -> m

tock :: Action -> Circuit -> (Circuit, [Action])
tock (src, pulse, dst) c = (c', todo)
  where
    (m, outs) = c M.! dst
    (m', mp) = tick src pulse m
    c' = c & at dst ?~ (m', outs)
    todo = case mp of
        Nothing -> []
        Just p -> (dst,p,) <$> outs

step :: Meter m -> (Seq Action, Circuit, m) -> (Seq Action, Circuit, m)
step _ (S.Empty, c, m) = (S.Empty, c, m)
step f (a :<| ax, c, m) = let (c', todo) = tock a c in (ax S.>< S.fromList todo, c', f a m)

push :: Circuit -> m -> (Seq Action, Circuit, m)
push = (S.singleton ("button", Low, "broadcaster"),,)

part1 :: Circuit -> Word
part1 = productOf each . (^. _2) . (`run` 1000)
  where
    run c = foldl' f (c, (0, 0)) . enumFromTo @Word 1
    f (c, (l, h)) _ = let (_, c', (l', h')) = go $ push c (0, 0) in (c', (l + l', h + h'))
    go s = let s' = step meter s in if s' ^. _1 & S.null then s' else go s'
    meter (_, p, _) (l, h) = if p == Low then (l + 1, h) else (l, h + 1)

part2 :: Circuit -> Word
part2 = run . setup
  where
    setup c = push c (M.fromList $ (,0) <$> starts c, 0)
    starts =
        concatMap ((\case Conjunction ms -> M.keys ms; _otherMod -> []) . fst)
            . filter (andOf each . bimap isConj ("rx" `elem`))
            . M.elems
    run (ax, c, (cs, i))
        | all (> 0) (M.elems cs) = foldl' lcm 1 $ M.elems cs
        | S.null ax = run $ push c (cs, i)
        | otherwise = run $ step meter (ax, c, (cs, i))
    meter (_, p, dst) (cs, i) = case (cs ^. at dst, p) of
        (Just _, High) -> (cs & at dst ?~ i, i + 1)
        _noneOrLow -> (cs, i + 1)

main :: IO ()
main = do
    input <- readCircuit <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
