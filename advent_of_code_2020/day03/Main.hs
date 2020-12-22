module Main
  ( main
  ) where

import Relude.Extra (member)

data Grid = Grid
  { _trees :: Set (Int, Int)
  , _maxR  :: Int
  , _maxD  :: Int
  }

input :: IO Grid
input = do
  raw <- readFileText "input.txt"
  let rows   = toString <$> lines raw
      zipped = zip [0 ..] $ fmap (zip [0 ..]) rows
      trees  = fromList [ (v, h) | (v, row) <- zipped, (h, c) <- row, c == '#' ]
      maxR   = fromMaybe 0 $ viaNonEmpty (length . head) rows
      maxD   = length rows
  pure $ Grid trees maxR maxD

run :: Grid -> (Int, Int) -> Int
run (Grid trees maxR maxD) (r, d) = go (0, 0) 0
 where
  go p@(v, h) c =
    let c' = c + bool 0 1 (member p trees)
        v' = v + d
        h' = (h + r) `mod` maxR
    in  if v' > maxD then c' else go (v', h') c'

part1 :: Grid -> Int
part1 g = run g $ (3, 1)

part2 :: Grid -> Int
part2 g = product $ run g <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
