module Main
  ( main
  ) where

import           Data.Attoparsec.Text
import           Data.Graph.DGraph    (DGraph)
import qualified Data.Graph.DGraph    as D
import           Data.Graph.Traversal (dfsVertices)
import           Data.Graph.Types     (Arc (..))

bagP :: Parser Text
bagP = do
  s <- many1' letter <* skipSpace
  c <- many1' letter <* skipSpace <* (string "bags" <|> string "bag")
  pure . fromString $ s <> " " <> c

sinksP :: Parser [(Int, Text)]
sinksP = (string "no other bags" $> []) <|> (p `sepBy1'` char ',')
  where p = (,) <$> (skipSpace *> decimal) <*> (skipSpace *> bagP)

arcsP :: Parser [Arc Text Int]
arcsP = do
  source <- bagP <* skipSpace <* string "contain" <* skipSpace
  sinks  <- sinksP <* char '.'
  pure $ fmap (\(n, sink) -> Arc source sink n) sinks

input :: IO (DGraph Text Int)
input =
  foldMap D.fromArcsList
    .   fromRight []
    .   parseOnly ((arcsP `sepBy1'` char '\n') <* skipSpace <* endOfInput)
    <$> readFileText "input.txt"

part1 :: DGraph Text Int -> Int
part1 graph = pred . length $ dfsVertices (D.transpose graph) "shiny gold"

part2 :: DGraph Text Int -> Int
part2 graph = go "shiny gold"
 where
  go v =
    let es = D.outboundingArcs graph v
    in  if null es then 0 else sum $ fmap (\(Arc _ y n) -> n * (1 + go y)) es

main :: IO ()
main = (bitraverse_ print print . (part1 &&& part2)) =<< input
