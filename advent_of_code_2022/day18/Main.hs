{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (readFile)

import Control.Lens
import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.Ix (inRange, range)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.IO (readFile)
import Linear.V3

type Drop = V3 Int

readDrops :: Text -> Set Drop
readDrops = either (error "Bad parse") S.fromList . parseOnly (c `sepBy1'` endOfLine)
  where
    c = V3 <$> decimal <*> (char ',' *> decimal) <*> (char ',' *> decimal)

neighbors :: Drop -> Set Drop
neighbors d = S.fromList [d & xyz +~ n | xyz <- [_x, _y, _z], n <- [-1, 1]]

part1 :: Set Drop -> Int
part1 drops = sum . fmap (\d -> 6 - S.size (S.intersection drops $ neighbors d)) $ S.toList drops

-- with help from https://work.njae.me.uk/2022/12/19/advent-of-code-2022-day-18/

part2 :: Set Drop -> Int
part2 drops = part1 exterior
  where
    exterior = S.fromList cube `S.difference` steam
    steam = fill (S.singleton $ head cube) S.empty
    cube = range box
    box = (V3 xLo yLo zLo, V3 xHi yHi zHi)
    get minMax xyz  = drops & minMax (folded . xyz)
    (xLo, yLo, zLo) = (get minimum1Of _x, get minimum1Of _y, get minimum1Of _z) & each %~ pred
    (xHi, yHi, zHi) = (get maximum1Of _x, get maximum1Of _y, get maximum1Of _z) & each %~ succ
    fill edge seen
        | null edge = seen
        | otherwise =
            let x = S.findMin edge
                xs =
                    S.filter
                        ( \n ->
                            S.notMember n drops
                                && S.notMember n edge
                                && S.notMember n seen
                                && inRange box n
                        )
                        $ neighbors x
             in fill (S.union xs $ S.delete x edge) (S.insert x seen)

main :: IO ()
main = do
    coords <- readDrops <$> readFile "input.txt"
    traverse_ (print . ($ coords)) [part1, part2]
