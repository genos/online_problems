{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bool (bool)
import Data.Either (fromRight)
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Vector (Vector)
import qualified Data.Vector as V

rd :: Text -> Int
rd = fromRight 0 . (fmap fst . T.signed T.decimal)

rdProg :: Text -> Vector Int
rdProg = V.fromList . fmap rd . T.split (== ',')

step :: (Int, Vector Int) -> Maybe (Vector Int, (Int, Vector Int))
step (i, xs)
  | o == 99 = Nothing
  | o == 1 || o == 2 = Just (xs', (i + 4, xs'))
  | otherwise = error "unknown opcode"
  where
    o = xs V.! i
    a = xs V.! (i + 1)
    b = xs V.! (i + 2)
    c = xs V.! (i + 3)
    x = xs V.! a
    y = xs V.! b
    z = bool (x * y) (x + y) (o == 1)
    xs' = xs V.// [(c, z)]

start :: Int -> Int -> Vector Int -> Vector Int
start noun verb = (V.// [(1, noun), (2, verb)])

run :: Vector Int -> Int
run = V.head . last . unfoldr step . (0, )

part1 :: Vector Int -> Int
part1 = run . start 12 2

part2 :: Vector Int -> Int
part2 xs =
  head
    [ 100 * noun + verb
    | noun <- [0 .. 99]
    , verb <- [0 .. 99]
    , 19690720 == run (start noun verb xs)
    ]

main :: IO ()
main = do
  xs <- rdProg <$> T.readFile "input"
  print $ part1 xs
  print $ part2 xs
