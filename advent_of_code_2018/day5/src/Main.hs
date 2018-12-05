module Main where

import Data.Char (isLetter, toLower)

react :: String -> Int
react = length . foldr step ""
 where
  step x (y : ys) | x /= y && toLower x == toLower y = ys
  step x ys       = x : ys

part1 :: String -> Int
part1 = react

part2 :: String -> Int
part2 s = minimum $ fmap (react . remove) ['a' .. 'z']
  where remove c = filter ((/= c) . toLower) s

main :: IO ()
main = do
  input <- filter isLetter <$> readFile "input"
  print . part1 $ input
  print . part2 $ input
