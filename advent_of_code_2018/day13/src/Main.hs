{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Monad.Cont     (callCC, runCont)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.Loops    (iterateM_, iterateUntilM)
import           Data.Bifunctor         (bimap, first, second)
import           Data.Either            (partitionEithers)
import           Data.Foldable          (traverse_)
import           Data.Function          (on)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M

--              /      \      +
data Corner = Fore | Back | Inter

data Direction = U | L | D | R

type YX = (Int, Int)
type Cart = (Direction, Int)
type Track = Map YX Corner
type Carts = Map YX Cart

move :: Direction -> YX -> YX
move U = first (subtract 1)
move L = second (subtract 1)
move D = first (+ 1)
move R = second (+ 1)

rotL, rotR, fore, back :: Direction -> Direction
rotL U = L
rotL L = D
rotL D = R
rotL R = U
rotR L = U
rotR D = L
rotR R = D
rotR U = R
fore U = R
fore R = U
fore L = D
fore D = L
back U = L
back L = U
back D = R
back R = D

parse :: String -> (Track, Carts)
parse input = bimap M.fromList M.fromList . partitionEithers $! do
  (y, line) <- zip [0 ..] $ lines input
  (x, c   ) <- zip [0 ..] line
  case c of
    '/'  -> [Left ((y, x), Fore)]
    '\\' -> [Left ((y, x), Back)]
    '+'  -> [Left ((y, x), Inter)]
    '^'  -> [Right ((y, x), (U, 0))]
    '<'  -> [Right ((y, x), (L, 0))]
    'v'  -> [Right ((y, x), (D, 0))]
    '>'  -> [Right ((y, x), (R, 0))]
    _    -> []

step :: Monad m => (YX -> m ()) -> Track -> Carts -> m Carts
step collide track = tick M.empty
 where
  tick past (M.minViewWithKey -> Just ((k, (o, n)), future))
    | M.member k' past || M.member k' future
    = collide k' >> (tick `on` M.delete k') past future
    | otherwise
    = tick (M.insert k' v past) future
   where
    k' = move o k
    v  = case M.lookup k' track of
      Nothing   -> (o, n)
      Just Fore -> (fore o, n)
      Just Back -> (back o, n)
      Just Inter ->
        let f = case n `mod` 3 of
              0 -> rotL
              1 -> id
              2 -> rotR
              _ -> error "inconceivable"
        in  (f o, n + 1)
  tick past _ = return past

day13a :: (Track, Carts) -> YX
day13a (track, carts) =
  runCont (callCC $ \f -> iterateM_ (step f track) carts) id

day13b :: (Track, Carts) -> YX
day13b (track, carts) =
  fst
    .  M.findMin
    .  runIdentity
    $! iterateUntilM ((== 1) . M.size) (step (const $! return ()) track) carts

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  traverse_ (putStrLn . (\(y, x) -> show y <> "," <> show x) . ($! input))
            [day13a, day13b]
