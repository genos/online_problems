module Main
  ( main
  ) where

import qualified Data.List.NonEmpty as NE

input :: IO (NonEmpty Int)
input =
  fromList
    .   sort
    .   rights
    .   fmap (readEither . toString)
    .   lines
    <$> readFileText "input.txt"

solve :: (f Int -> Int) -> (f Int -> Int) -> NonEmpty (f Int) -> Maybe Int
solve mul add = viaNonEmpty head . fmap mul . NE.filter ((== 2020) . add)

part1 :: NonEmpty Int -> Maybe Int
part1 xs = solve (uncurry (*)) (uncurry (+)) $ (,) <$> xs <*> xs

part2 :: NonEmpty Int -> Maybe Int
part2 xs =
  solve (\(a, b, c) -> a * b * c) (\(a, b, c) -> a + b + c)
    $   (,,)
    <$> xs
    <*> xs
    <*> xs

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
