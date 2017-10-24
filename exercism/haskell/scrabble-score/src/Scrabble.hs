module Scrabble (scoreLetter, scoreWord) where

import Data.Char       (toUpper)
import Data.Map.Strict (Map, findWithDefault, fromList)

table :: Map Char Integer
table = fromList
  [ (c, i)
  | (cs, i) <-
    [ ("AEIOULNRST", 1)
    , ("DG"        , 2)
    , ("BCMP"      , 3)
    , ("FHVWY"     , 4)
    , ("K"         , 5)
    , ("JX"        , 8)
    , ("QZ"        , 10)
    ]
  , c <- cs
  ]

scoreLetter :: Char -> Integer
scoreLetter = flip (findWithDefault 0) table . toUpper

scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
