{-# LANGUAGE DataKinds  #-}
module Main
  ( main
  ) where

import           Data.Attoparsec.Text
import           Data.Foldable        (maximum)
import           Data.Vector.Sized    (Vector)
import qualified Data.Vector.Sized    as V

data FrontBack = F | B deriving stock (Eq, Ord, Enum, Bounded, Show)

parseFB :: Char -> Maybe FrontBack
parseFB = \case
  'F' -> Just F
  'B' -> Just B
  _   -> Nothing

data LeftRight = L | R deriving stock (Eq, Ord, Enum, Bounded, Show)

parseLR :: Char -> Maybe LeftRight
parseLR = \case
  'L' -> Just L
  'R' -> Just R
  _   -> Nothing

data BoardingPass = BoardingPass
  { _row :: Vector 7 FrontBack
  , _col :: Vector 3 LeftRight
  }
  deriving stock (Eq, Show)

parser :: Parser (Maybe BoardingPass)
parser = do
  fb <- mapMaybe parseFB <$> count 7 (char 'F' <|> char 'B')
  lr <- mapMaybe parseLR <$> count 3 (char 'L' <|> char 'R')
  pure $ BoardingPass <$> V.fromList fb <*> V.fromList lr

input :: IO [BoardingPass]
input =
  catMaybes
    .   fromRight []
    .   parseOnly ((parser `sepBy1'` char '\n') <* skipSpace <* endOfInput)
    <$> readFileText "input.txt"

reducer :: Enum a => Vector n a -> Int
reducer = V.foldl' (\n x -> 2 * n + fromEnum x) 0

seatID :: BoardingPass -> Int
seatID = (\c r -> c + 8 * r) <$> reducer . _col <*> reducer . _row

part1 :: [BoardingPass] -> Int
part1 = maximum . fmap seatID

part2 :: [BoardingPass] -> Maybe Int
part2 bs = viaNonEmpty (succ . fst . head)
  $ filter (\(a, b) -> b - a == 2) pairs
 where
  pairs = zip (init ids) (tail ids)
  ids   = fromList . sort $ seatID <$> bs

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
