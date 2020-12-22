module Main
  ( main
  ) where

import           Data.IntMap   (findWithDefault, fromSet, (!))
import qualified Relude.Unsafe as U

input :: IO [Int]
input = do
  adapters <- rights . fmap (readEither . toString) . lines <$> readFileText "input.txt"
  let xs = sort adapters
  pure $ 0 : xs ++ [3 + U.last xs]

part1 :: [Int] -> Int
part1 chain = product . fmap length . group . sort $ zipWith (-) (U.tail chain) chain

-- https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day10.md
-- Too pretty to pass up!
part2 :: [Int] -> Int
part2 chain = m ! 0
 where
  m = flip fromSet (fromList chain) $ \i ->
    bool (sum [ findWithDefault 0 (i + j) m | j <- [1 .. 3] ]) 1 (i == U.last chain)

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
