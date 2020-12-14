module Main
  ( main
  ) where

import Data.Set  (intersection)
import Data.Text (splitOn)

input :: IO [[Text]]
input = fmap lines . splitOn "\n\n" <$> readFileText "input.txt"

part1 :: [[Text]] -> Int
part1 = sum . fmap (length . ordNub . toString . fold)

part2 :: [[Text]] -> Int
part2 = sum . fmap
  (length . foldl' intersection (fromList ['a' .. 'z']) . fmap
    (fromList . toString)
  )

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
