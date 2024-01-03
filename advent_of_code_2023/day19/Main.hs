{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens (Lens', each, productOf, sumOf, (%~), (.~), (^.), _1, _2, _3, _4)
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text.IO qualified as T

data Category = X | M | A | S
data Destination = Name !Text | Accept | Reject
data Rule = Immediate !Destination | Step {_predicate :: !(Category, Ordering, Word), _destination :: !Destination}
type Part t = (t, t, t, t)

cat :: Category -> Lens' (Part t) t
cat = \case X -> _1; M -> _2; A -> _3; S -> _4

readSetup :: Text -> (Map Text [Rule], [Part Word])
readSetup = either (error "Bad parse") id . parseOnly ((,) <$> ws <*> ("\n\n" *> ps <* endOfInput))
  where
    ws = M.fromList <$> workflow `sepBy1'` "\n"
    ps = part `sepBy1'` "\n"
    workflow = (,) <$> takeTill (== '{') <*> ("{" *> ((rule `sepBy1'` ",") <* "}"))
    part = (,,,) <$> ("{x=" *> decimal) <*> (",m=" *> decimal) <*> (",a=" *> decimal) <*> (",s=" *> decimal <* "}")
    rule = choice [Step <$> predicate <*> (":" *> destination), Immediate <$> destination]
    predicate = (,,) <$> choice [X <$ "x", M <$ "m", A <$ "a", S <$ "s"] <*> choice [LT <$ "<", GT <$ ">"] <*> decimal
    destination = choice [Accept <$ "A", Reject <$ "R", Name <$> takeTill (not . isAlpha)]

part1 :: Map Text [Rule] -> [Part Word] -> Word
part1 system = sum . fmap (sumOf each) . filter (accept $ Name "in")
  where
    accept Accept _ = True
    accept Reject _ = False
    accept (Name n) p = accept (head . mapMaybe (push p) $ system M.! n) p
    push _ (Immediate d) = Just d
    push p (Step (c, o, i) d) = if compare (p ^. cat c) i == o then Just d else Nothing

type Range = (Word, Word)

split :: Range -> Ordering -> Word -> (Maybe Range, Maybe Range)
split r@(l, h) o i = if o == LT then lt else gt
  where
    lt
        | h < i = (Just r, Nothing)
        | l > i = (Nothing, Just r)
        | h == i = (Just (l, i - 1), Just (i, i))
        | l == i = (Nothing, Just (i, h))
        | l < i && h > i = (Just (l, i - 1), Just (i, h))
        | otherwise = (Nothing, Nothing)
    gt
        | l > i = (Just r, Nothing)
        | h < i = (Nothing, Just r)
        | h == i = (Nothing, Just (l, i))
        | l == i = (Just (i + 1, h), Just (i, i))
        | l < i && h > i = (Just (i + 1, h), Just (l, i))
        | otherwise = (Nothing, Nothing)

part2 :: Map Text [Rule] -> Word
part2 system = numAccept (Name "in") (full, full, full, full)
  where
    full = (1, 4_000)
    numAccept Accept p = p & each %~ width & productOf each
    numAccept Reject _ = 0
    numAccept (Name n) p = sum . fmap (uncurry numAccept) $ percolate (Just p) (system M.! n)
    width (l, h) = if l > h then 0 else h + 1 - l
    percolate _ [] = []
    percolate Nothing _ = []
    percolate (Just p) (Immediate d : _) = [(d, p)]
    percolate (Just p) ((Step (c, o, w) Accept) : rs) = ((Accept,) <$> maybeToList yes) <> percolate no rs
      where
        (yes, no) = yesNo p c o w
    percolate (Just p) ((Step (c, o, w) Reject) : rs) = percolate no rs
      where
        (_, no) = yesNo p c o w
    percolate (Just p) ((Step (c, o, w) (Name n)) : rs) = percolate yes (system M.! n) <> percolate no rs
      where
        (yes, no) = yesNo p c o w
    update p c x = p & cat c .~ x
    yesNo p c o w = let (good, bad) = split (p ^. cat c) o w in (update p c <$> good, update p c <$> bad)

main :: IO ()
main = do
    (system, parts) <- readSetup <$> T.readFile "input.txt"
    print $ part1 system parts
    print $ part2 system
