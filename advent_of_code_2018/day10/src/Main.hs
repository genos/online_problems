{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Monad        (ap)
import qualified Data.Attoparsec.Text as P
import           Data.Bifunctor       (bimap, second)
import           Data.Bool            (bool)
import           Data.Ix              (range)
import           Data.Semigroup       (Max (..), Min (..))
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Linear.V2

type Point    = V2 Int
type Velocity = V2 Int

lineP :: P.Parser (Point, Velocity)
lineP = do
  x <- P.string "position=<" *> P.skipSpace *> P.signed P.decimal
  y <- P.char ',' *> P.skipSpace *> P.signed P.decimal <* P.char '>'
  v <- P.skipSpace *> P.string "velocity=<" *> P.skipSpace *> P.signed P.decimal
  w <- P.char ',' *> P.skipSpace *> P.signed P.decimal <* P.char '>'
  pure (V2 x y, V2 v w)

parseInput :: IO ([Point], [Velocity])
parseInput = do
  input <- T.readFile "input"
  case P.parseOnly (lineP `P.sepBy` P.char '\n') input of
    Left  l  -> error l
    Right ps -> pure $ unzip ps

step :: [Velocity] -> [Point] -> [Point]
step = zipWith (+)

boundingBox :: Foldable f => f Point -> (Point, Point)
boundingBox ps = (V2 xMin yMin, V2 xMax yMax)
 where
  (Min xMin, Min yMin, Max xMax, Max yMax) =
    foldMap (\(V2 x y) -> (Min x, Min y, Max x, Max y)) ps

clusterArea :: [Point] -> Int
clusterArea (boundingBox -> (mins, maxs)) = product $ maxs - mins

lastDecreasingBy :: Ord a => (b -> a) -> [b] -> b
lastDecreasingBy _ [] = error "can't do this for empty"
lastDecreasingBy f xs =
  snd . last . takeWhile (uncurry (>=) . bimap f f) $ (zip `ap` tail) xs

message :: [Velocity] -> [Point] -> (Int, Set Point)
message vs =
  second S.fromList
    . lastDecreasingBy (clusterArea . snd)
    . zip [0 ..]
    . iterate (step vs)

pretty :: Set Point -> Text
pretty ps = T.unlines
  [ T.pack [ bool '░' '▓' (V2 x y `S.member` ps) | x <- range (xMin, xMax) ]
  | y <- range (yMin, yMax)
  ]
  where (V2 xMin yMin, V2 xMax yMax) = boundingBox ps


main :: IO ()
main = do
  (xs, vs) <- parseInput
  let (steps, msg) = message vs xs
  T.putStrLn $ pretty msg
  print steps
