{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed (embedFile)
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Vector (Vector)
import qualified Data.Vector as V

input :: ByteString
input = $(embedFile "input.txt")

type Bit = Int
type Vec = Vector Bit
type Screen = Matrix Bit

screen :: Int -> Int -> Screen
screen = M.zero

rotVec :: Int -> Vec -> Vec
rotVec n v = y V.++ x where
  n' = (- n) `mod` V.length v
  (x, y) = V.splitAt n' v

rect :: Int -> Int -> Screen -> Screen
rect cs rs s = M.elementwise (.|.) s t where
  t = M.setSize 0 (M.nrows s) (M.ncols s) $ fmap (+1) (M.zero rs cs)

rotRow :: Int -> Int -> Screen -> Screen
rotRow i rs s = M.mapRow f i s where
  r = rotVec (rs + 1) $ M.getRow i s
  m = V.length r
  f j _ = r V.! (j `mod` m)

rotCol :: Int -> Int -> Screen -> Screen
rotCol j cs s = M.mapCol f j s where
  c = rotVec (cs + 1) $ M.getCol j s
  n = V.length c
  f i _ = c V.! (i `mod` n)

data Move = Rect { _rows :: Int, _cols :: Int }
          | RotRow { _y :: Int, _rSteps ::  Int }
          | RotCol { _x :: Int, _cSteps :: Int }
  deriving (Eq, Show)

move :: Screen -> Move -> Screen
move s (Rect rs cs) = rect rs cs s
move s (RotRow y rSteps) = rotRow y rSteps s
move s (RotCol x cSteps) = rotCol x cSteps s

countLit :: Screen -> Int
countLit = sum

parseRect :: AC.Parser Move
parseRect = do
    _ <- AC.string "rect "
    rows <- AC.decimal
    _ <- AC.char 'x'
    cols <- AC.decimal
    return $ Rect rows cols

parseRotCol :: AC.Parser Move
parseRotCol = do
  _ <- AC.string "rotate column x="
  x <- AC.decimal
  _ <- AC.string " by "
  cSteps <- AC.decimal
  return $ RotCol (x + 1) cSteps

parseRotRow :: AC.Parser Move
parseRotRow = do
  _ <- AC.string "rotate row y="
  y <- AC.decimal
  _ <- AC.string " by "
  rSteps <- AC.decimal
  return $ RotRow (y + 1) rSteps


parseMove :: AC.Parser Move
parseMove = parseRect <|> parseRotCol <|> parseRotRow

moves :: ByteString -> [Move]
moves =
  map (either error id . AC.parseOnly parseMove) . BC.lines

pretty :: Screen -> ByteString
pretty = BC.unlines . map BC.pack . M.toLists . fmap (" X" !!)

test1 :: Bool
test1 = a && b && c where
  a = pretty testScreen == "       \n       \n       \n"
  b = testMoves == [Rect 3 2, RotCol 2 1, RotRow 1 4, RotCol 2 1]
  c = map pretty (scanl move testScreen testMoves) ==
        [ "       \n       \n       \n"
        , "XXX    \nXXX    \n       \n"
        , "X X    \nXXX    \n X     \n"
        , "    X X\nXXX    \n X     \n"
        , " X  X X\nX X    \n X     \n"
        ]
  testScreen = screen 3 7
  testMoves = moves testInput
  testInput = BC.unlines [ "rect 3x2"
                         , "rotate column x=1 by 1"
                         , "rotate row y=0 by 4"
                         , "rotate column x=1 by 1"
                         ]


output :: Screen
output = foldl move (screen 6 50) . moves $ input

part1 :: Int
part1 = countLit output

part2 :: ByteString
part2 = pretty output

main :: IO ()
main = do
  print test1
  print part1
  BC.putStrLn part2
