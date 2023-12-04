{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text hiding (take)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as I
import Data.List (intersect)
import Data.Sequence qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T

type Card = (Int, [Int], [Int])

readCards :: Text -> [Card]
readCards = either (error "Bad parse") id . parseOnly ((card `sepBy1'` "\n") <* endOfInput)
  where
    card = (,,) <$> ("Card" *> skipSpace *> decimal <* ": ") <*> nums <*> (" | " *> nums)
    nums = (skipSpace *> decimal) `sepBy1'` " "

part1 :: [Card] -> Int
part1 = sum . fmap score
  where
    score (_, w, h) = let i = w `intersect` h in if null i then 0 else (2 ^) . pred $ length i

part2 :: [Card] -> Int
part2 cards = go 0 (S.fromList $ fmap (\(n, _, _) -> n) cards)
  where
    wins = I.fromList $ fmap (\(n, w, h) -> (n, S.fromList $ take (length $ w `intersect` h) [n + 1 ..])) cards
    go n S.Empty = n
    go n xs@(y S.:<| _) =
        let (ys, zs) = S.spanl (== y) xs
            ws = foldMap (wins I.!) ys
         in go (n + length ys) (S.unstableSort $ ws <> zs)

main :: IO ()
main = do
    input <- readCards <$> T.readFile "input.txt"
    traverse_ (print . ($ input)) [part1, part2]
