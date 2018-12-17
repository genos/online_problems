{-|
Module:         Day13
Description:    <https://adventofcode.com/2018/day/13 Day 13: Mine Cart Madness>
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
module Main
where

import           Control.Arrow          (first, second, (***))
import           Control.Monad.Cont     (callCC, runCont)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.Loops    (iterateM_, iterateUntilM)
import           Data.Either            (partitionEithers)
import           Data.Function          (on)
import           Data.Map.Lazy          (Map)
import qualified Data.Map.Lazy          as Map (delete, empty, findMin,
                                                fromList, insert, lookup,
                                                member, minViewWithKey, size)

data Crossing = Reflect | Reflect' | Spin

data Orientation = North | West | South | East

move :: Num a => Orientation -> (a, a) -> (a, a)
move North = first (subtract 1)
move West  = second (subtract 1)
move South = first (+ 1)
move East  = second (+ 1)

rotL, rotR, reflect, reflect' :: Orientation -> Orientation
rotL North = West
rotL West  = South
rotL South = East
rotL East  = North
rotR West  = North
rotR South = West
rotR East  = South
rotR North = East
reflect North = East
reflect East  = North
reflect West  = South
reflect South = West
reflect' North = West
reflect' West  = North
reflect' South = East
reflect' East  = South

parse
  :: (Integral i)
  => String
  -> (Map (i, i) Crossing, Map (i, i) (Orientation, Int))
parse input = (Map.fromList *** Map.fromList) . partitionEithers $ do
  (y, line) <- zip [0 ..] $ lines input
  (x, c   ) <- zip [0 ..] line
  case c of
    '/'  -> [Left ((y, x), Reflect)]
    '\\' -> [Left ((y, x), Reflect')]
    '+'  -> [Left ((y, x), Spin)]
    '^'  -> [Right ((y, x), (North, 0))]
    '<'  -> [Right ((y, x), (West, 0))]
    'v'  -> [Right ((y, x), (South, 0))]
    '>'  -> [Right ((y, x), (East, 0))]
    _    -> []

step
  :: (Integral i, Monad m)
  => ((i, i) -> m ())
  -> Map (i, i) Crossing
  -> Map (i, i) (Orientation, Int)
  -> m (Map (i, i) (Orientation, Int))
step notifyCollision crossings = step' Map.empty
 where
  step' past (Map.minViewWithKey -> Just ((k, (o, n)), future))
    | Map.member k' past || Map.member k' future
    = notifyCollision k' >> (step' `on` Map.delete k') past future
    | otherwise
    = step' (Map.insert k' v past) future
   where
    k' = move o k
    v  = case Map.lookup k' crossings of
      Nothing       -> (o, n)
      Just Reflect  -> (reflect o, n)
      Just Reflect' -> (reflect' o, n)
      Just Spin     -> (spins !! (n `mod` length spins) $ o, n + 1)
  step' past _ = return past
  spins = [rotL, id, rotR]

day13a :: String -> String
day13a (parse @Int -> (crossings, cars)) = show x ++ "," ++ show y
  where (y, x) = runCont (callCC $ \f -> iterateM_ (step f crossings) cars) id

day13b :: String -> String
day13b (parse @Int -> (crossings, cars)) = show x ++ "," ++ show y
 where
  ((y, x), _) = Map.findMin $ runIdentity $ iterateUntilM
    ((== 1) . Map.size)
    (step (const $ return ()) crossings)
    cars


main :: IO ()
main = do
  input <- readFile "input"
  putStrLn $! day13a input
  putStrLn $! day13b input

-- {-# LANGUAGE LambdaCase      #-}
-- module Main where

-- import           Control.Monad.State
-- import           Data.Bifunctor        (bimap)
-- import           Data.Either           (partitionEithers)
-- import           Data.Map.Strict       (Map)
-- import qualified Data.Map.Strict       as M

-- data YX = YX { _y :: {-# UNPACK #-}!Int, _x :: {-# UNPACK #-}!Int } deriving Eq

-- instance Ord YX where
--   (YX y x) `compare` (YX y' x') = compare y y' <> compare x x'

-- instance Show YX where
--   show (YX y x) = show x <> "," <> show y

-- --               ^   v   <   >
-- data Direction = U | D | L | R deriving Eq

-- --           \      /      +
-- data Curve = Back | Fore | Inter deriving Eq

-- data Cart = Cart { _direction :: Direction, _turns :: Word } deriving Eq

-- type Track = Map YX Curve
-- type Carts = Map YX Cart

-- parse :: String -> (Track, Carts)
-- parse input = bimap M.fromList M.fromList . partitionEithers $ do
--   (y, line) <- zip [0 ..] $ lines input
--   (x, c   ) <- zip [0 ..] line
--   let yx = YX y x
--   case c of
--     '\\' -> [Left (yx, Back)]
--     '/'  -> [Left (yx, Fore)]
--     '+'  -> [Left (yx, Inter)]
--     '^'  -> [Right (yx, Cart U 0)]
--     'v'  -> [Right (yx, Cart D 0)]
--     '<'  -> [Right (yx, Cart L 0)]
--     '>'  -> [Right (yx, Cart R 0)]
--     _    -> []

-- rotL :: Direction -> Direction
-- rotL U = L
-- rotL D = R
-- rotL L = D
-- rotL R = U

-- rotR :: Direction -> Direction
-- rotR U = R
-- rotR D = L
-- rotR L = U
-- rotR R = D

-- back :: Direction -> Direction
-- back U = L
-- back D = R
-- back L = U
-- back R = D

-- fore :: Direction -> Direction
-- fore U = R
-- fore D = L
-- fore L = D
-- fore R = U

-- move :: Direction -> YX -> YX
-- move U (YX y x) = YX (y - 1) x
-- move D (YX y x) = YX (y + 1) x
-- move L (YX y x) = YX y (x - 1)
-- move R (YX y x) = YX y (x + 1)

-- tick :: Track -> State Carts [YX]
-- tick track = do
--   carts <- get
--   fmap snd . M.toAscList <$> M.traverseMaybeWithKey (step track) carts

-- step :: Track -> YX -> Cart -> State Carts (Maybe YX)
-- step track yx (Cart direction turns) = gets (M.member yx) >>= \case
--   False -> pure Nothing
--   True  -> do
--     modify' $! M.delete yx
--     let yx' = move direction yx
--     gets (M.member yx') >>= \case
--       True -> modify (M.delete yx') >> pure (Just yx')
--       False -> do
--         case M.lookup yx' track of
--           Nothing    -> modify' id
--           Just Back  -> modify' $! M.insert yx' (Cart (back direction) turns)
--           Just Fore  -> modify' $! M.insert yx' (Cart (fore direction) turns)
--           Just Inter -> let d = case turns `mod` 3 of
--                                   0 -> rotL direction
--                                   1 -> direction
--                                   2 -> rotR direction
--                                   _ -> error "inconceivable"
--                         in modify' $! M.insert yx' (Cart d (turns + 1))
--         pure Nothing

-- part1 :: Track -> Carts -> YX
-- part1 track carts = evalState go carts
--   where
--     go = tick track >>= \case
--       []     -> go
--       (yx:_) -> pure yx

-- part2 :: Track -> Carts -> YX
-- part2 track carts = evalState go carts
--   where
--     go = tick track >> gets ((== 1) . M.size) >>= \case
--       False -> go
--       True  -> gets (fst . M.elemAt 0)

-- main :: IO ()
-- main = do
--   (track, carts) <- parse <$> readFile "input"
--   print $! part1 track carts
--   print $! part2 track carts
