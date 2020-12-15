module Main
  ( main
  ) where

input :: IO [Int]
input =
  sort . rights . fmap (readEither . toString) . lines <$> readFileText "input.txt"

solve :: (f Int -> Int) -> (f Int -> Int) -> [f Int] -> Maybe Int
solve mul add = viaNonEmpty head . fmap mul . filter ((== 2020) . add)

part1 :: [Int] -> Maybe Int
part1 xs = solve (uncurry (*)) (uncurry (+)) $ (,) <$> xs <*> xs

part2 :: [Int] -> Maybe Int
part2 xs =
  solve (\(a, b, c) -> a * b * c) (\(a, b, c) -> a + b + c) $ (,,) <$> xs <*> xs <*> xs

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
