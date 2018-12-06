module Main where

import           Control.Arrow        ((&&&))
import           Control.Lens         ((^.))
import qualified Data.Attoparsec.Text as P
import           Data.Ix
import           Data.List            (sortOn)
import qualified Data.Map.Strict      as M
import qualified Data.Text.IO         as T
import           Linear.V2

type Vec = V2 Int

vec :: P.Parser Vec
vec = V2 <$> P.decimal <*> (P.char ',' *> P.space *> P.decimal)

taxicab :: Vec -> Vec -> Int
taxicab v w = sum . abs $ v - w

bounds :: [Vec] -> (Int, Int, Int, Int)
bounds vs = (minimum xs, maximum xs, minimum ys, maximum ys)
 where
  xs = fmap (^. _x) vs
  ys = fmap (^. _y) vs

grid :: [Vec] -> [Vec]
grid vs = range (V2 xMin yMin, V2 xMax yMax)
  where (xMin, xMax, yMin, yMax) = bounds vs

closest :: [Vec] -> Vec -> [Vec]
closest vs v = [c0 | d0 /= d1]
 where
  cabs                  = sortOn fst $ fmap (taxicab v &&& id) vs
  [(d0, c0), (d1, _c1)] = take 2 cabs

area :: [Vec] -> Int
area vs = maximum ms
 where
  closests = concatMap (closest vs) $ grid vs
  (xMin, xMax, yMin, yMax) = bounds vs
  within (V2 x y) = x /= xMin && x /= xMax && y /= yMin && y /= yMax
  ms = M.fromListWith (+) . fmap (id &&& const 1) $ filter within closests

main :: IO ()
main = do
  input <- T.readFile "input"
  let vecs = case P.parseOnly (vec `P.sepBy` P.char '\n') input of
        Left  err -> error err
        Right vs  -> vs
  print $ area vecs
