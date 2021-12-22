{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import           Data.Bifunctor              (bimap)
import           Data.Char                   (ord)
import           Data.Foldable               (for_, traverse_)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

type Polymer = [Char]  -- aka String
type Rules = Map (Char, Char) Char
type Counter = Vector Word

readPolymerization :: Text -> (Polymer, Rules)
readPolymerization = either (error "Bad parse") (bimap T.unpack M.fromList)
  . parseOnly ((,) <$> template <*> (rules `sepBy1` "\n"))
 where
  template = takeTill isEndOfLine <* "\n\n"
  rules    = (,) <$> ((,) <$> letter <*> letter) <*> (" -> " *> letter)

step :: Rules -> Polymer -> Polymer
step !rules !polymer = (p :) . concatMap lookupAndSplice $ zip polymer ps
 where
    !p = head polymer
    !ps = tail polymer
    lookupAndSplice (!a, !b) = case rules M.!? (a, b) of
      Nothing -> []
      Just !x -> [x, b]

toCounter :: Polymer -> Counter
toCounter polymer = V.create $ do
  !v <- M.replicate 26 0
  for_ polymer $ \p -> do
    let !i = ord p - 65 -- ord 'A'
    M.unsafeModify v succ i
  pure v

iterateN :: Word -> (a -> a) -> a -> a
iterateN 0 _ !x = x
iterateN !n !f !x =  let !x' = f x
                         !n' = n - 1
                      in iterateN n' f x'

range :: Counter -> Word
range = (-) <$> V.maximum <*> (V.minimum . V.filter (> 0))

solve :: Word -> (Rules, Polymer) -> Word
solve !n (!rules, !polymer) = range . toCounter $ iterateN n (step rules) polymer

part1 :: (Rules, Polymer) -> Word
part1 = solve 10

part2 :: (Rules, Polymer) -> Word
part2 = solve 40

main :: IO ()
main = do
  (!polymer, !rules) <- readPolymerization <$> T.readFile "test.txt"
  traverse_ (print . ($ (rules, polymer))) [part1, part2]
