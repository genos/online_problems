module Main where

import           Data.Fixed    (mod')
import           Data.Foldable (traverse_)
import           Data.Stream   (Stream)
import qualified Data.Stream   as S

rng147 :: Double -> Stream Double
rng147 = S.iterate (\x -> (147 * x) `mod'` 1)

main :: IO ()
main = traverse_ print . S.take 10 . rng147 $ pi / 10
