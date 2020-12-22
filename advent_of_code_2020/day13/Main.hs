module Main
  ( main
  ) where

import           Data.Foldable (minimumBy)
import qualified Data.Text     as T
import           GHC.Base      (until)

input :: IO (Int, [(Int, Int)])
input = do
  raw <- readFileText "input.txt"
  let (a, bs) = second (zip [0 ..] . T.splitOn "," . T.strip) $ T.breakOn "\n" raw
      ts      = mapMaybe (traverse (readMaybe . toString)) bs
  pure $ maybe (error "Bad parse of first line") (, ts) (readMaybe $ toString a)

part1 :: (Int, [(Int, Int)]) -> Int
part1 (t, ts) = uncurry (*) $ minimumBy
  (comparing snd)
  [ (time, minutes) | (_, time) <- ts, let minutes = time - (t `mod` time) ]

-- crt in disguise
part2 :: (Int, [(Int, Int)]) -> Int
part2 = fst . foldl' go (0, 1) . snd
 where
  go (base, step) (offset, m) =
    let base' = until (\n -> (n + offset) `mod` m == 0) (+ step) base
    in  (base', step * m)

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
