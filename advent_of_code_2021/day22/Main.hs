{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl', traverse_)
import           Data.Function        ((&))
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text.IO         as T

type ReactorCore = Set (Int, Int, Int)
type Step = ReactorCore -> ReactorCore

turn
  :: (ReactorCore -> ReactorCore -> ReactorCore)  -- union or difference
  -> (Int, Int)  -- x bounds
  -> (Int, Int)  -- y bounds
  -> (Int, Int)  -- z bounds
  -> Bool  -- whether to clamp
  -> Step
turn op (xLo, xHi) (yLo, yHi) (zLo, zHi) clamp core = op core $ S.unions
  [ S.singleton (x, y, z)
  | x <- [f xLo .. f xHi]
  , y <- [f yLo .. f yHi]
  , z <- [f zLo .. f zHi]
  ]
  where f = if clamp then max (-50) . min 50 else id

on, off :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool -> Step
on = turn S.union
off = turn S.difference

readSteps :: Bool -> Text -> [Step]
readSteps clamp = fromRight (error "Bad parse") . parseOnly (step `sepBy1'` "\n")
 where
  step = do
    f  <- on <$ "on" <|> off <$ "off"
    xs <- (,) <$> (" x=" *> signed decimal) <*> (".." *> signed decimal)
    ys <- (,) <$> (",y=" *> signed decimal) <*> (".." *> signed decimal)
    zs <- (,) <$> (",z=" *> signed decimal) <*> (".." *> signed decimal)
    pure $ f xs ys zs clamp

run :: [Step] -> Int
run = S.size . foldl' (&) S.empty

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  traverse_ (print . (\b -> run $ readSteps b input)) [True, False]
